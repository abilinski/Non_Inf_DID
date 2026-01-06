* This program produces the figures in the paper.;

#delimit ;
clear all;
capture log close;

/*
CHANGE PATH(BELOW) TO LOCATION OF DATA;
*/

cd "E:\Projects\CA\DependentCoverage\asako\test_replication_package\AEJEPcode\data";
set more 1;
set mem 900m;
log using depcov_obtain_figures.log,replace;


************************************************************
Figure1. Percentage of Young People with Any Insurance Coverage by Treatment and Control Groups 
************************************************************;

use depcov_dataset;

* create treatment group variable.;
gen treat=.;
replace treat=1 if age>=19 & age<=25;
replace treat=0 if ((age>=16 & age<=18)|(age>=27 & age<=29));

* create quarter variable.;
gen qtr=.;
replace qtr=1 if month>=1 & month<=3;
replace qtr=2 if month>=4 & month<=6;
replace qtr=3 if month>=7 & month<=9;
replace qtr=4 if month>=10 & month<=12;

keep anyhi p_weight year qtr treat;

save data_for_figure, replace;

* obtain the rate of coverage for treatment group.;
use data_for_figure;
collapse (mean) anyhi_1=anyhi [weight=p_weight] if treat==1, by(year qtr);
save anyhi_1, replace;
sort year qtr;

* obtain the rate of coverage for control group.;
use data_for_figure;
collapse (mean) anyhi_0=anyhi [weight=p_weight] if treat==0, by(year qtr);
save anyhi_0, replace;
sort year qtr;

* merge the rates;
use anyhi_1;
merge year qtr using anyhi_0;
drop _merge;
sort year qtr;

save anyhi, replace;

* Create a graph;

gen anyhi_1_p=anyhi_1*100;
gen anyhi_0_p=anyhi_0*100;

drop anyhi_1 anyhi_0;
rename anyhi_1_p anyhi_1;
rename anyhi_0_p anyhi_0;

label variable anyhi_1 "19-25 yrs old, treatment";
label variable anyhi_0 "16-18, 27-29 yrs old, control";

gen time = tq(2004q1) + (year-2004)*4 +(qtr-1);
format %tq time;

drop if year<=2008;

graph twoway (line anyhi_1 anyhi_0 time, clwidth(medthick medthick) lpattern(solid shortdash) lcolor(black black)), 
ytitle(Percentage) xtitle(Year and Quarter) xline(200 202 204, lcolor(black black black)) graphregion(color(gs14));

graph export depcov_figure1.ps, replace;

* erase unnecessary dataset;
erase data_for_figure.dta;
erase anyhi.dta;
erase anyhi_1.dta;
erase anyhi_0.dta;

clear;

********************
***** Figure 2 *****
********************;
************************************************************
Figure 2. Percentage of Young People Covered by Employer Sponsored Health Insurance as Parents’ Dependents
************************************************************;

use depcov_dataset;

* create time variable.;
gen time=.;

replace time=1 if ((year<=2009)|(year==2010 & month<=2));
replace time=2 if (year==2010 & month>=3 & month<=9);
replace time=3 if (year==2010 & month>=10 & month<=12);
replace time=4 if (year==2011 & month>=1);

keep emphi_dep p_weight time age;

save data_for_figure, replace;

* obtain the rate of coverage by time.;

forvalues num=1/4{;
 use data_for_figure;
 collapse (mean) emphi_dep_`num'=emphi_dep [weight=p_weight] if 
 time==`num', by(age);
 save emphi_dep_`num', replace;
 sort age;
};

* merge the rates;

use emphi_dep_1;
merge age using emphi_dep_2 emphi_dep_3 emphi_dep_4;
drop _merge*;

save emphi_dep, replace;

* Create graphs;

gen emphi_dep_1_p=emphi_dep_1*100;
gen emphi_dep_2_p=emphi_dep_2*100;
gen emphi_dep_3_p=emphi_dep_3*100;
gen emphi_dep_4_p=emphi_dep_4*100;

drop emphi_dep_1 emphi_dep_2 emphi_dep_3 emphi_dep_4;
rename emphi_dep_1_p emphi_dep_1;
rename emphi_dep_2_p emphi_dep_2;
rename emphi_dep_3_p emphi_dep_3;
rename emphi_dep_4_p emphi_dep_4;

label variable emphi_dep_1 "Aug 2008- Feb 2010";
label variable emphi_dep_2 "Mar-Sep 2010";
label variable emphi_dep_3 "Oct-Dec 2010";
label variable emphi_dep_4 "Jan-Nov 2011";

graph twoway (line emphi_dep_1 emphi_dep_2 emphi_dep_3 emphi_dep_4 age,
clwidth(medthick medthick medthick medthick) lpattern(solid shortdash_dot longdash shortdash) lcolor(black black black black)),
xline(19 26, lcolor(black black)) graphregion(color(gs14)) xmtick(##5) ytitle("Percentage") xtitle("Age") ;
graph export depcov_figure2.ps, replace;


* erase unnecessary dataset;
  erase emphi_dep_1.dta;
  erase emphi_dep_2.dta;
  erase emphi_dep_3.dta;
  erase emphi_dep_4.dta;
  erase emphi_dep.dta;



clear;
exit;

