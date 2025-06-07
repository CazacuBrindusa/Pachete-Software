options validvarname=v7;

/*
 =============================================
 1. import the csv into work.qol
 =============================================
*/
proc import
    datafile="/home/u64208740/Proiect/quality_of_life_indices_by_country.csv"
    out=work.qol
    dbms=csv
    replace;
    guessingrows=MAX;
run;

/*
 =============================================
 2. quick check of imported data
 =============================================
*/
proc contents data=work.qol; run;
proc print    data=work.qol(obs=10) noobs; run;

/*
 ================================================
 3. define format for quality-of-life categories
 ================================================
*/
proc format;
    value qol_cat_fmt
        low  - <60   = 'low'
        60   - <70   = 'medium'
        70   - high  = 'high';
run;

/*
 =============================================
 4. analyze missing values
 =============================================
*/
proc freq data=work.qol;
    tables _character_ / missing;
run;
proc means data=work.qol n nmiss mean median;
    var _numeric_;
run;

/*
 =============================================
 5. simple median imputation
 =============================================
*/
proc stdize
    data=work.qol
    out=work.qol_imputed
    method=median
    reponly;
    var _numeric_;
run;

/*
 ===========================================================================
 6. data step: apply category format, rescale indexes, create binary target
 ===========================================================================
*/
data work.qol_prep;
    set work.qol_imputed;
    format quality_of_life_index qol_cat_fmt.;
    qol_cat = put(quality_of_life_index, qol_cat_fmt.);
    array idxs{*} safety_index pollution_index cost_of_living_index;
    do i = 1 to dim(idxs);
        idxs{i} = round(idxs{i}/10, 0.01);
    end;
    drop i;
    high_qol = (quality_of_life_index > 70);
run;

/*
 =============================================
 7. create subsets by qol_cat
 =============================================
*/
data work.qol_low work.qol_medium work.qol_high;
    set work.qol_prep;
    if qol_cat = 'low'     then output work.qol_low;
    else if qol_cat = 'medium' then output work.qol_medium;
    else if qol_cat = 'high'    then output work.qol_high;
run;

/*
 =============================================
 8. summary report with proc report
 =============================================
*/
proc report data=work.qol_prep nowd;
    column 
        qol_cat 
        quality_of_life_index 
        safety_index 
        pollution_index 
        cost_of_living_index;

    define qol_cat               / group      'qol category';
    define quality_of_life_index / analysis   mean format=8.2 'avg qol';
    define safety_index          / analysis   mean format=8.2;
    define pollution_index       / analysis   mean format=8.2;
    define cost_of_living_index  / analysis   mean format=8.2;
run;

/*
 ===========================================================
 9. correlations among key variables with overridden cutoff
 ===========================================================
*/
proc corr data=work.qol_prep plots(maxpoints=100000)=matrix(nvar=all);
    var quality_of_life_index safety_index pollution_index
        cost_of_living_index health_care_index traffic_commute_time_index;
run;

/*
 ==========================================================
 10. scatter plot of safety_index vs quality_of_life_index
 ==========================================================
*/
proc sgplot data=work.qol_prep;
    scatter x=safety_index y=quality_of_life_index / markerattrs=(symbol=circlefilled);
    reg     x=safety_index y=quality_of_life_index;
    title 'qol vs safety';
run;

/*
 =============================================
 11. classification with proc logistic
 =============================================
*/
/* 11.1 split into train and test */
%let train_pct = 0.7;
proc surveyselect
    data=work.qol_prep
    out=work.qol_split
    outall
    method=srs
    rate=&train_pct
    seed=12345;
run;

data work.train work.test;
    set work.qol_split;
    if selected then output work.train;
    else             output work.test;
run;

/* 11.2 build list of predictors */
proc sql noprint;
    select name into :indepvars separated by ' '
    from dictionary.columns
    where libname='WORK' and memname='QOL_PREP'
      and upcase(name) not in (
          'QUALITY_OF_LIFE_INDEX','QOL_CAT','HIGH_QOL',
          'COUNTRY','RANK','CLIMATE_INDEX','YEAR'
      );
quit;

/* 11.3 fit logistic model and save */
proc logistic 
     data=work.train 
     descending 
     outmodel=work.logit_model 
     plots=none;
  model high_qol(event='1') = &indepvars;
  score data=work.test out=work.test_pred;
  score data=work.test outroc=work.roc;
run;

/* 11.4 find the cutoff (PROB) */
proc sql noprint;
  create table work.bestcut as
    select _PROB_ as cutoff,
           (_SENSIT_ - _1MSPEC_) as youden
    from work.roc;
  select cutoff into :best_cutoff trimmed
    from work.bestcut
    having youden = max(youden);
quit;

/* 11.5 apply a 0.5 cutoff */
data work.test_pred2;
  set work.test_pred;
  pred_05 = (P_1 > 0.5);
run;

/* 11.6 confusion matrix at cutoff=0.5 */
proc freq data=work.test_pred2;
  tables high_qol*pred_05 / nocol nopercent;
  title 'logistic confusion matrix (cutoff=0.5)';
run;

/*
 =============================================
 12. decision tree with proc hpsplit
 =============================================
*/
proc hpsplit data=work.train seed=2025;
    class high_qol;
    model high_qol(event='1') = &indepvars;
    grow entropy;
    prune costcomplexity;
    code file="/home/u64208740/Proiect/qol_tree.sas";
run;

data work.tree_scored;
    set work.test;
    %include "/home/u64208740/Proiect/qol_tree.sas";
    pred_tree05 = (p_high_qol1 > 0.5);
run;

proc freq data=work.tree_scored;
    tables high_qol*pred_tree05 / nocol nopercent;
    title 'tree confusion matrix';
run;
