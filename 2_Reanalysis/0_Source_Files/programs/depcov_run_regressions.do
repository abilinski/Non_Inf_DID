* This program produces the results appear as tables in the paper.;

#delimit ;
clear all;
capture log close;
/*
CHANGE PATH(BELOW) TO LOCATION OF DATA;
*/

cd "YOUR WD";
set more 1;
set mem 900m;
log using depcov_run_regressions.log,replace;

use depcov_dataset;

********************
* create variables *
********************;

* age dummies;
forvalues num=16/25{;
gen age`num'= age==`num';
};
forvalues num=27/29{;
gen age`num'= age==`num';
};

* year dummies;
tab year, gen (year);
rename year2 y2009;
rename year3 y2010;
rename year4 y2011;

* month dummies;
forvalues m=1/12{;
gen month`m'= month==`m';
};

* trend variable;
gen trend = (year-2008)*12 +(month-8);

* post-reform dummies;
gen mar_sep10=(year==2010 & month>=3 & month<=9);
gen after_oct10=( (year==2010 & month>=10) |year>=2011);

gen oct10_feb11=( (year==2010 & month>=10) | (year==2011 & month<=2) );
gen mar11_sept11 = (year==2011 & month>=3 & month<=9); 
gen after_sept11 = ((year==2011 & month>9) |year>2011);

gen after_mar10=(year==2010 & month>=3 | year>=2011);

* a dummy for treatment group;
gen fedelig = age>=19 & age <26;

* interactions of post-reform dummies and a dummy for treatment group;
gen elig_mar10=mar_sep10*fedelig;
gen elig_oct10=after_oct10*fedelig;

gen elig_middle=oct10_feb11*fedelig;
gen elig_marsept=mar11_sept11*fedelig;
gen elig_sept11=after_sept11*fedelig;

* interaction of unemployment rate and treatment group dummy.;
gen ue_treat=ue*fedelig;

* interactions of post-reform dummies and an indicator for parent's ESI coverage;
gen emppar_mar10=emphi_parent*mar_sep10;
gen emppar_oct10=emphi_parent*after_oct10;

* an interaction of a treatment group dummy and an indicator for parent's ESI coverage;
gen fedelig_emppar=fedelig*emphi_parent;

* interactions of post-reform dummies, treatment group dummy, and an indicator for parent's ESI coverage;
gen ddd_mar10=mar_sep10*fedelig*emphi_parent;
gen ddd_oct10=after_oct10*fedelig*emphi_parent;

* labor force status variable;
gen employed=(labor==1);
gen unemployed=(labor==2);
gen ln_hours=ln(hours+1) if hours>-1;
gen hour_vary=(hours==-1);

* create age group variable.;
gen age_group=.;
replace age_group=1 if age>=16 & age<=18;
replace age_group=2 if age>=19 & age<=25;
replace age_group=3 if age>=27 & age<=29;

** Add state linear trend;
tab fipstate;
foreach st in 1 2 4 5 6 8 9 10 11 12 13 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 44 45 46 47 48 49 50 51 53 54 55 56{;
gen st_trend_`st'=0;
replace st_trend_`st'=trend if fipstate==`st';
};

gen trend_treat = trend*fedelig;

gen unins=anyhi==0;

* create family coverage variable, which indicates whether a parent had family coverage in Nov 2009 -Feb 2010 given that a parent had ESI in that period and the young adult did not have dependent coverage through parent's ESI;
gen famcov_pre=.;
replace famcov_pre=1 if other_dep_pre==1 & emphi_dep_pre==0;
replace famcov_pre=0 if other_dep_pre==0 & emphi_p_pre==1 & emphi_dep_pre==0;

* Notes: other_dep_pre=1 indicates that other dependent was covered by parent's ESI in Nov 2009 -Feb 2010.;
* emphi_p_pre==1 indicates that a parent had ESI in Nov 2009 -Feb 2010.;
* emphi_dep_pre==0 indicates that the young adult was not covered by parent's ESI in Nov 2009 -Feb 2010.;

* Create placeo law variables;
forvalues n=1/18{;
gen placebo`n'=(trend>=`n'); 
gen placebo`n'_treat=placebo`n'*fedelig;
};

**********************************************
sum_group variable indicates 
(pre or post law) * (treatment or control). 
This is used when unconditional DD is obtained. 
**********************************************;

gen sum_group=.;
replace sum_group=1 if after_oct10==0 & mar_sep10 ==0 & fedelig==1;
replace sum_group=2 if after_oct10==0 & mar_sep10 ==0 & fedelig==0;
replace sum_group=3 if after_oct10==1 & fedelig==1;
replace sum_group=4 if after_oct10==1 & fedelig==0;


********************************************
* define macros for expalanatory variables.*
********************************************;

local effects mar_sep10 after_oct10 fedelig elig_mar10 elig_oct10;

local exp_vars age17-age18  age20-age25 age27-age29 trend female ue hispanic white asian other student mar fpl_ratio fpl_ratio_2 ue_treat y2009 y2010 month2-month12 st_trend_1-st_trend_55;

local effects_three mar_sep10 oct10_feb11 mar11_sept11 after_sept11 elig_mar10 elig_middle elig_marsept elig_sept11;

local exp_vars_sub trend ue fpl_ratio fpl_ratio_2 ue_treat y2009 y2010 month2-month12 st_trend_1-st_trend_55;

local effects_ddd mar_sep10 after_oct10 fedelig emphi_parent elig_mar10 elig_oct10 emppar_mar10 emppar_oct10 fedelig_emppar ddd_mar10 ddd_oct10;

local exp_vars_ddd i.emphi_parent*i.age trend female ue hispanic white asian other student mar fpl_ratio fpl_ratio_2 ue_treat i.emphi_parent*y2009 i.emphi_parent*y2010 i.emphi_parent*y2011 i.month st_trend_1-st_trend_55;

local var_list employed unemployed age female white black hispanic mar ft_student hsdo hsg somcol colgrd  fpl_ratio bad_hlth live_wparent; 

local exp_trend fedelig trend trend_treat age17-age18 age20-age25 age27-age29 female ue hispanic white asian other student mar fpl_ratio fpl_ratio_2 ue_treat y2009 y2010 month2-month12 st_trend_1-st_trend_55;

local exp_vars_placebo age17-age18 age20-age25 age27-age29 trend female ue hispanic white asian other student mar fpl_ratio fpl_ratio_2 ue_treat y2009 month2-month12 st_trend_1-st_trend_55;

save augmented, replace;

******************************
* Obtain tables in the paper.*
******************************;

di "Table 1. Demographic, Socioeconomic and Insurance Characteristics"; 

* all observations;
sum anyhi emphi_dep emphi indiv govhi employed unemployed age white black hispanic mar ft_student hsdo hsg somcol colgrd bad_hlth [weight=p_weight] ;

* by age group;
forvalues k =1/3{;
sum anyhi emphi_dep emphi indiv govhi employed unemployed age white black hispanic mar ft_student hsdo hsg somcol colgrd bad_hlth [weight=p_weight] if age_group==`k';
};

tempfile data_reg;
save `data_reg', replace;

di "Count number of unique persons (last row of Table 1)";

keep groupid age_group;
gen count=1;

tempfile unique_obs;
save `unique_obs', replace;

* all population;
collapse (sum) count, by (groupid);
sum; * number of observations indicates the number of unique persons;

*** 16-18 years old***;
clear;
use `unique_obs';

keep if age_group==1;
collapse (sum) count, by (groupid);
sum;

*** 19-25 years old***;
clear;
use `unique_obs';

keep if age_group==2;
collapse (sum) count, by (groupid);
sum;

***27-29 years old***;
clear;
use `unique_obs';

keep if age_group==3;
collapse (sum) count, by (groupid);
sum;

clear;
use `data_reg';

di "Table 2: Effect of ACA on Coverage of Young Adults 19-25 years: Main DD Results (regression)";
foreach instype in anyhi emphi_dep indiv emphi govhi{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight],absorb(fipstate) cluster(fipstate);
};

di "Table 2: Effect of ACA on Coverage of Young Adults 19-25 years: Main DD Results (unconditional DD)";
forvalues k=1/4{;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if sum_group==`k';
};


di "Table 3: Effect of ACA on Coverage of Young Adults 19-25 years: Three post-ACA time periods, DD results (regression)";
foreach instype in anyhi emphi_dep indiv emphi govhi{;
xi: areg `instype' `effects_three' `exp_vars' [weight=p_weight], absorb(fipstate) cluster(fipstate);
};

di "Table 3: Effect of ACA on Coverage of Young Adults 19-25 years: Three post-ACA time periods, DD results (unconditional DD)";

gen sum_group3=.;
replace sum_group3=1 if mar_sep10==0 & oct10_feb11==0 & mar11_sept11==0 & after_sept11==0 & fedelig==1;
replace sum_group3=2 if mar_sep10==0 & oct10_feb11==0 & mar11_sept11==0 & after_sept11==0 & fedelig==0;
replace sum_group3=3 if  mar_sep10==1& fedelig==1;
replace sum_group3=4 if  mar_sep10==1 & fedelig==0;
replace sum_group3=5 if  oct10_feb11==1& fedelig==1;
replace sum_group3=6 if  oct10_feb11==1 & fedelig==0;
replace sum_group3=7 if  mar11_sept11==1& fedelig==1;
replace sum_group3=8 if  mar11_sept11==1 & fedelig==0;
replace sum_group3=9 if  after_sept11==1 & fedelig==1;
replace sum_group3=10 if  after_sept11==1 & fedelig==0;

forvalues k=1/10{;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if sum_group3==`k';
};

di "Table 4: Effect of ACA on Coverage of Young Adults 19-25 years: DD results by Subgroups";

di "by Age group: 19-22 years old";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age22 female hispanic white asian other student mar `exp_vars_sub' [weight=p_weight] if age<=22, absorb(fipstate) cluster(fipstate);
};

di "by Age group: 23-25 years old";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age24-age25 age27-age29 female hispanic white asian other student mar `exp_vars_sub' [weight=p_weight] if age>=23, absorb(fipstate) cluster(fipstate);
};

di "by Gender: Male";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age25 age27-age29 hispanic white asian other student mar `exp_vars_sub' [weight=p_weight] if female==0, absorb(fipstate) cluster(fipstate);
};

di "by Gender: Female";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age25 age27-age29 hispanic white asian other student mar `exp_vars_sub' [weight=p_weight] if female==1, absorb(fipstate) cluster(fipstate);
};

di "by Race/Ethnicity: White";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age25 age27-age29 female student mar `exp_vars_sub' [weight=p_weight] if white==1, absorb(fipstate) cluster(fipstate);
};

di "by Race/Ethnicity: Non-White";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic asian other student mar `exp_vars_sub' [weight=p_weight] if white==0, absorb(fipstate) cluster(fipstate);
};

di "by Marital status: married";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic white asian other student `exp_vars_sub' [weight=p_weight] if mar==1, absorb(fipstate) cluster(fipstate);
};

di "by Marital status: Non-married";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic white asian other student `exp_vars_sub' [weight=p_weight] if mar==0, absorb(fipstate) cluster(fipstate);
};

di "by Student status: Full-time students";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic white asian other mar `exp_vars_sub' [weight=p_weight] if ft_student==1, absorb(fipstate) cluster(fipstate);
};

di "by Student status: Others";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic white asian other mar `exp_vars_sub' [weight=p_weight] if ft_student==0, absorb(fipstate) cluster(fipstate);
};

di "by Health status: Excellent";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if bad_hlth==0, absorb(fipstate) cluster(fipstate);
};

di "by Health status: Less than excellent";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if bad_hlth==1, absorb(fipstate) cluster(fipstate);
};

di "by State law status: States that enacted laws";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if st_with_law==1, absorb(fipstate) cluster(fipstate);
};

di "by State law status: States that never enacted laws";
foreach instype in anyhi emphi_dep{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if st_with_law==0, absorb(fipstate) cluster(fipstate);
};


di "Table 4: Test the significance of the difference between subgroups";

eststo clear;
foreach instype in anyhi emphi_dep {;

di "by Age group: 19-22 years old";
xi: reg `instype' `effects' age17-age18 age20-age22 female hispanic white asian other student mar `exp_vars_sub' i.fipstate[weight=p_weight] if age>=16 & age<=22;
estimates store `instype'_yngctrl;

di "by Age group: 23-25 years old";
xi: reg `instype' `effects' age24-age25 age27-age29 female hispanic white asian other student mar `exp_vars_sub' i.fipstate[weight=p_weight] if age>=23 & age<=29;
estimates store `instype'_oldctrl;

suest `instype'_yngctrl `instype'_oldctrl, cluster(fipstate);
test ([`instype'_yngctrl_mean]_b[elig_oct10] = [`instype'_oldctrl_mean]_b[elig_oct10]);
test ([`instype'_yngctrl_mean]_b[elig_mar10] = [`instype'_oldctrl_mean]_b[elig_mar10]);


di "by Gender: Male";
xi: reg `instype' `effects' age17-age18 age20-age25 age27-age29 hispanic white asian other student mar `exp_vars_sub' i.fipstate[weight=p_weight] if female==0;
estimates store `instype'_men;

di "by Gender: Female";
xi: reg `instype' `effects' age17-age18 age20-age25 age27-age29 hispanic white asian other student mar `exp_vars_sub' i.fipstate[weight=p_weight] if female==1;
estimates store `instype'_women;

suest `instype'_men `instype'_women, cluster(fipstate);
test ([`instype'_men_mean]_b[elig_oct10] = [`instype'_women_mean]_b[elig_oct10]);
test ([`instype'_men_mean]_b[elig_mar10] = [`instype'_women_mean]_b[elig_mar10]);


di "by Race/Ethnicity: White";
xi: reg `instype' `effects' age17-age18 age20-age25 age27-age29 female student mar `exp_vars_sub' i.fipstate[weight=p_weight] if white==1;
estimates store `instype'_white;

di "by Race/Ethnicity: Non-White";
xi: reg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic asian other student mar `exp_vars_sub' i.fipstate[weight=p_weight] if white==0;
estimates store `instype'_nonwhite;

suest `instype'_white `instype'_nonwhite, cluster(fipstate);
test ([`instype'_white_mean]_b[elig_oct10] = [`instype'_nonwhite_mean]_b[elig_oct10]);
test ([`instype'_white_mean]_b[elig_mar10] = [`instype'_nonwhite_mean]_b[elig_mar10]);


di "by Marital status: Non-married";
xi: reg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic white asian other student  `exp_vars_sub' i.fipstate[weight=p_weight] if mar==0;
estimates store `instype'_unmarried;

di "by Marital status: Married";
xi: reg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic white asian other student  `exp_vars_sub' i.fipstate[weight=p_weight] if mar==1;
estimates store `instype'_married;

suest `instype'_unmarried `instype'_married, cluster(fipstate);
test ([`instype'_unmarried_mean]_b[elig_oct10] = [`instype'_married_mean]_b[elig_oct10]);
test ([`instype'_unmarried_mean]_b[elig_mar10] = [`instype'_married_mean]_b[elig_mar10]);


di "by Student status: Full-time students";
xi: reg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic white asian other mar `exp_vars_sub' i.fipstate [weight=p_weight] if ft_student==1;
estimates store `instype'_ftstudent;

di "by Student status: Others";
xi: reg `instype' `effects' age17-age18 age20-age25 age27-age29 female hispanic white asian other mar `exp_vars_sub' i.fipstate [weight=p_weight] if ft_student==0;
estimates store `instype'_nonftstudent;

suest `instype'_ftstudent `instype'_nonftstudent, cluster(fipstate);
test ([`instype'_ftstudent_mean]_b[elig_oct10] = [`instype'_nonftstudent_mean]_b[elig_oct10]);
test ([`instype'_ftstudent_mean]_b[elig_mar10] = [`instype'_nonftstudent_mean]_b[elig_mar10]);


di "by Health status: Excellent";
xi: reg `instype' `effects' `exp_vars' mar i.fipstate [weight=p_weight] if  bad_hlth==0;
estimates store `instype'_good;

di "by Health status: Less than excellent";
xi: reg `instype' `effects' `exp_vars' mar i.fipstate [weight=p_weight] if  bad_hlth==1;
estimates store `instype'_bad;

suest `instype'_good `instype'_bad, cluster(fipstate);
test ([`instype'_good_mean]_b[elig_oct10] = [`instype'_bad_mean]_b[elig_oct10]);
test ([`instype'_good_mean]_b[elig_mar10] = [`instype'_bad_mean]_b[elig_mar10]);


di "by State law status: States that enacted laws";
xi: reg `instype' `effects' `exp_vars' mar i.fipstate [weight=p_weight] if st_with_law==1;
estimates store `instype'_law;

di "by State law status: States that never enacted laws";
xi: reg `instype' `effects' `exp_vars' mar i.fipstate [weight=p_weight] if st_with_law==0;
estimates store `instype'_nolaw;

suest `instype'_law `instype'_nolaw, cluster(fipstate);
test ([`instype'_law_mean]_b[elig_oct10] = [`instype'_nolaw_mean]_b[elig_oct10]);
test ([`instype'_law_mean]_b[elig_mar10] = [`instype'_nolaw_mean]_b[elig_mar10]);

};


di "Table 5. Effect of ACA on Coverage of Young Adults 19-25 years: DDD Results using parental information (regression)";
foreach instype in anyhi emphi_dep indiv emphi govhi{;
xi: areg `instype' `effects_ddd' `exp_vars_ddd'[weight=p_weight],absorb(fipstate) cluster(fipstate);
};

di "Table 5: Effect of ACA on Coverage of Young Adults 19-25 years: DDD Results using parental information  (unconditional DDD)";
forvalues k=1/4{;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if sum_group==`k' & emphi_parent==0;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if sum_group==`k' & emphi_parent==1;
};


di "Table 6: Characteristics of Uninsured (but eligible) Young adults";

di " Table 6, Column 1: those who took up parental ESI";
sum `var_list' [weight=p_weight] if emphi_dep==1 & emphi_parent==1 & fedelig==1 & year==2011 & (month<=11 & month>=8);

di " Table 6, Column 2: uninsured and have parents with ESI.";
sum `var_list' [weight=p_weight] if anyhi==0 & emphi_parent==1 & fedelig==1 & year==2011 & (month<=11 & month>=8);

di "Table 6, Column3: Test the sig of difference";

foreach var in `var_list' {;
reg `var' unins [weight=p_weight] if (anyhi==0 |emphi_dep==1) & emphi_parent==1 & fedelig==1 & year==2011 & (month<=11 & month>=8);
};

di "Table 7. Effect of ACA on Labor Market Outcome of Young Adults 19-25 years: DD Results (regression)";

di "Table 7 Columns 1, 2, and 5-7";
foreach labor_dep in employed ft job_ease emp2diffemp hour_vary{;
xi: areg `labor_dep' `effects' `exp_vars' [weight=p_weight], absorb(fipstate) cluster(fipstate);
};

di "Table 7 Columns 3 & 4";

foreach labor_dep in hours ln_hours{;
xi: areg `labor_dep' `effects' `exp_vars' [weight=p_weight] if hours>-1, absorb(fipstate) cluster(fipstate);
};

di "Table 7. Effect of ACA on Labor Market Outcome of Young Adults 19-25 years: DD Results (unconditional DD)";

di "Table 7 Columns 1, 2, and 5-7";
forvalues k=1/4{;
summarize employed ft job_ease emp2diffemp hour_vary [weight=p_weight] if sum_group==`k';
};

di "Table 7 Columns 3 & 4";

forvalues k=1/4{;
summarize hours ln_hours [weight=p_weight] if hours>-1 & sum_group==`k';
};

di "Appendix Table A1. Test for Equality of Pre-Reform Trends between Control and Treatment Groups";

di "any coverage";
xi: areg anyhi `exp_trend' [weight=p_weight] if (year<=2009 | (year==2010 & month<=2)), absorb(fipstate) cluster(fipstate);

di "dependent coverage through parents";
xi: areg emphi_dep `exp_trend' [weight=p_weight] if (year<=2009 | (year==2010 & month<=2)), absorb(fipstate) cluster(fipstate);

di "Appendix Table A2. The Availability of Parent’s Information, by Age and Age Group, Post Reform Enactment";

tab age after_mar10, sum(info_parent);
tab age_group after_mar10, sum(info_parent);


di "Appendix Table A3. DD Results for sample with Parental Health Insurance Status";

di "Regression results";
foreach instype in anyhi emphi_dep indiv emphi govhi{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if emphi_parent !=., absorb(fipstate) cluster(fipstate);
};

di "Unconditional DD";
forvalues k=1/4{;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if emphi_parent !=. & sum_group==`k';
};

di "Appendix Table A4. The Effect of the ACA Dependent Provision on Parent’s Own ESI Coverage";

di "Regression results";
xi: areg emphi_parent `effects' `exp_vars' [weight=p_weight], absorb(fipstate) cluster(fipstate);

di "Unconditional DD";
forvalues k=1/4{;
summarize emphi_parent [weight=p_weight] if sum_group==`k';
};

di "Appendix Table A5. Descriptive Results of Marginal Cost Analysis: Family vs. non-family coverage (August 2011- November 2011)";

summarize emphi_dep [weight=p_weight] if famcov_pre==1 & year==2011 & (month<=11 & month>=8) & fedelig==1 & emphi_parent==1;
summarize emphi_dep [weight=p_weight] if famcov_pre==0 & year==2011 & (month<=11 & month>=8) & fedelig==1 & emphi_parent==1;

di "Appendix Table A6. Placebo Test Results: Randomly Selected Months between September 2008 and January 2010";

* focus on before aca period;
drop if (year==2010 & month>=3) | year>=2011;  

forvalues n=1/18{;
 foreach instype in anyhi emphi_dep emphi indiv govhi{;
  xi: areg `instype' placebo`n' fedelig placebo`n'_treat `exp_vars_placebo' [weight=p_weight],absorb(fipstate) 
   cluster(fipstate);
 };
};

clear;
use `data_reg';

di "Unreported Table 1: Effect of ACA on Spousal Coverage (regression)";
xi: areg emphi_dep_spous `effects' `exp_vars' [weight=p_weight], absorb(fipstate) cluster(fipstate);
xi: areg emphi_dep_spous `effects' `exp_vars' [weight=p_weight] if mar==1, absorb(fipstate) cluster(fipstate);

di "Unreported Table 1: Effect of ACA on Spousal Coverage (unconditional DD)";
forvalues k=1/4{;
summarize emphi_dep_spous [weight=p_weight] if sum_group==`k';
summarize emphi_dep_spous [weight=p_weight] if mar==1 & sum_group==`k';
};


di "Unreported Table 2: Effect of ACA on Coverage of Young Adults 19-25 years: Young Control Group (Regression)";
foreach instype in anyhi emphi_dep indiv emphi govhi{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if age<=25, absorb(fipstate) cluster(fipstate);
};

di "Unreported Table 2: Effect of ACA on Coverage of Young Adults 19-25 years: Young Control Group (unconditional DD)";
forvalues k=1/4{;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if age<=25 & sum_group==`k';
};

di "Unreported Table 3: Effect of ACA on Coverage of Young Adults 19-25 years: Older Control Group (Regression)";
foreach instype in anyhi emphi_dep indiv emphi govhi{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if age>=19, absorb(fipstate) cluster(fipstate);
};

di "Unreported Table 3: Effect of ACA on Coverage of Young Adults 19-25 years: Older Control Group (unconditional DD)";
forvalues k=1/4{;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if age>=19 & sum_group==`k';
};


di "Unreported Table 4. Effect of ACA on Coverage of Young Adults 19-25 years:  States That Passed State Dependent Coverage Laws During August 2008 to February 2010 Are Excluded (Regression)";

* create a enact_09_11 variable, indicates state law was enacted in the state between 2009 and 2011.;
gen enact_09_11=0;
foreach st in 9 17 22 36 39 42 53 55{;
replace enact_09_11=1 if fipstate==`st';
};

foreach instype in anyhi emphi_dep indiv emphi govhi{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if enact_09_11==0, absorb(fipstate) cluster(fipstate);
};

di "Unreported Table 4. Effect of ACA on Coverage of Young Adults 19-25 years:  States That Passed State Dependent Coverage Laws During August 2008 to February 2010 Are Excluded (unconditional DD)";
forvalues k=1/4{;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if enact_09_11==0 & sum_group==`k';
};


di "Unreported Table 5. Effect of ACA on Coverage of Young Adults 19-25 years: Massachusetts Is Excluded (Regression)";
foreach instype in anyhi emphi_dep indiv emphi govhi{;
xi: areg `instype' `effects' `exp_vars' [weight=p_weight] if fipstate!=25, absorb(fipstate) cluster(fipstate);
};

di "Unreported Table 5. Effect of ACA on Coverage of Young Adults 19-25 years: Massachusetts Is Excluded (unconditional DD)";
forvalues k=1/4{;
summarize anyhi emphi_dep indiv emphi govhi [weight=p_weight] if fipstate!=25 & sum_group==`k';
};



clear;
exit;
