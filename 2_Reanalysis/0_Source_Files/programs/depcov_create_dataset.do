* This program creates dataset for the analysis of dependent coverage.;

#delimit ;
clear all;

/*
CHANGE PATH(BELOW) TO LOCATION OF DATA;
*/

cd "E:\Projects\CA\DependentCoverage\asako\test_replication_package\AEJEPcode\data";
set more 1;
set mem 900m;
capture log close;
log using depcov_create_dataset.log,replace;

************************
*** Merge dataset ******
************************;

*Obtain state unemployment, monthly;

use stateuerate_monthy_81_2011.dta; 

keep fipstate year month ue;
sort year month fipstate;
save stateue,replace;

*Obtin a state with law dummy. A dummy variable that indicates that the state implemented law at some point (before or during the data period, 2008-2011).;
use st_with_law.dta; 
sort fipstate;
tempfile st_with_l;
save `st_with_l',replace;


*SIPP dataset;
* Obtain self-reported health from Wave 4 Topical Module.;

local var_health ssuid epppnum ehltstat;
use `var_health' using sippp08putm4.dta;                     

rename ssuid suid;      
rename epppnum pnum;
rename ehltstat health;

destring pnum, replace;

sort suid pnum;
tempfile health_tm4;
save `health_tm4',replace;


* Obtain Wave 1-10 Core files.;

* list of variables to keep;

local var_keep srefmon ssuid epppnum swave tfipsst rhcalmn rhcalyr esex eorigin erace tage ecrmth ecdmth ehimth ehiowner rchampm ehemply tftotinc efnp epnspous enonhh wpfinwgt ems rmesr eeducate eenrlm renroll epnmom epndad eeno1 ejbhrs1 ejbhrs2 shhadid;

use `var_keep' using sippl08puw1.dta;                     
keep if srefmon==4;  
save temp_w1.dta, replace; * I created temp**.dta because I was not sure how I can expand my temprary folder in Stata.; 

local i=2;
while `i'<=10 {;
use sippl08puw`i'.dta;
keep `var_keep';
keep if srefmon==4;                      
save temp_w`i'.dta, replace;                      
local i=`i'+1;
};

use temp_w1.dta;
local i=2;
while `i'<=10 {;
append using temp_w`i'.dta;
erase temp_w`i'.dta;
local i=`i'+1;
};
erase temp_w1.dta;


* rename variables.;

rename tfipsst fipstate;
rename rhcalmn month;
rename rhcalyr year;
rename ssuid suid;      
rename swave wave;      
rename epppnum pnum;
rename tage age;
rename efnp famnum;
rename epnspous pnum_spouse;
rename wpfinwgt p_weight;
rename enonhh cov_other;
rename tftotinc famyinc;
rename epnmom pnum_mom;
rename epndad pnum_dad;
rename eeno1 empnum;
rename shhadid adid;

* destring pnum because the values of other variables such as pnum of a spouse are numeric.;

destring pnum, replace;

                        ****************************************************;

* Merge unemployment rates;
sort year month fipstate;
merge year month fipstate using stateue;
drop if _merge==2;
drop _merge;

* Merge a dummy variable that indicates that the state implemented law at some point.;
sort fipstate;
merge fipstate using `st_with_l';
drop if _merge==2;
drop _merge;

* Merge self-reported health status;
sort suid pnum;
merge suid pnum using `health_tm4';
drop if _merge==2;
drop _merge;

***************************
*** Create variables ******
***************************;


***** Demographic variables. *****;

* Create numeric person-specific id variable using group function.;
sort suid pnum;
egen groupid=group(suid pnum);

* define indicators for being married.;
gen mar= (ems==1 | ems==2);

* define an indicator for female;
gen female= (esex==2);

* define race indicators, black, hispanic, white, others.
* Note: the current way of coding is that someone can have "race" black or white and then "become" hispanic. So the categories are "white:non-hispanic", "black:non-hispanic", "asian: non-hispanic", "hispanic", and "other".;
 		
gen hispanic= (eorigin==1);
gen white = (eorigin!=1 & erace==1);
gen black = (eorigin!=1 & erace==2);
gen asian = (eorigin!=1 & erace==3);
gen other = (eorigin!=1 & erace==4);

* family income variable;
* Obtain the percentage of fpl using famyinc and famnum variables.;
* There is a variable, fpov, but there are many missing values and cannot be used.;

gen famyincy=famyinc*12;

gen my_fratio=.;
forval num = 1/30 {;
replace my_fratio=famyincy/(6130+3180*`num') if year==2004 & famnum==`num';
replace my_fratio=famyincy/(6310+3260*`num') if year==2005 & famnum==`num';
replace my_fratio=famyincy/(6400+3400*`num') if year==2006 & famnum==`num';
replace my_fratio=famyincy/(6730+3480*`num') if year==2007 & famnum==`num';
replace my_fratio=famyincy/(6800+3600*`num') if year==2008 & famnum==`num';
replace my_fratio=famyincy/(7090+3740*`num') if year==2009 & famnum==`num';
replace my_fratio=famyincy/(7090+3740*`num') if year==2010 & famnum==`num';
replace my_fratio=famyincy/(7070+3820*`num') if year==2011 & famnum==`num';
};

* create fpl_ratio variable (income as a share of federal poverty line) and its square term.;
gen fpl_ratio=.;
replace fpl_ratio=my_fratio;
replace fpl_ratio=0 if fpl_ratio<0;
gen fpl_ratio_2=fpl_ratio*fpl_ratio;


***** Health insurance variables. *****;

* Assign mutuallly exclusive insurance status variable.;
gen ins_status=.;
replace ins_status=1 if (ehimth==1 & (ehiowner==1|ehiowner==3) &  (1<=ehemply & ehemply<=3 ) & ins_status==.);
replace ins_status=2 if (ehimth==1 & ehiowner==2 & (1<=ehemply & ehemply<=3 ) & ins_status==.);
replace ins_status=3 if (ehimth==1 & ehemply==7 & ins_status==.);
replace ins_status=4 if (rchampm==1 & ins_status==.);
replace ins_status=5 if (ecdmth==1 & ins_status==.);
replace ins_status=6 if (ecrmth==1 & ins_status==.);
replace ins_status=7 if (ehimth==1 & ins_status==.);
replace ins_status=0 if ins_status==.;

* 1: employer provided health insurance in own name, 
2: employer provided health insurance by someone else's plan, 
3: individually purchased coverage (in own name and as a dependent), 
4:CHAMPUS program or CHAMPVA program, 
5: Medicaid coverage (SCHIP is included), 
6: Medicare coverage,
7: other private coverage;

* Indicator for employer provided health insurance in own name.;
gen emphi=(ins_status==1);

* Indicator for employer provided health insurance by someone else's plan.;
gen emphi_dep_all=(ins_status==2);

* Indicator for employer provided health insurance by parents' plan. -> later.;

* Indicator for individually purchased coverage in own name;
gen indiv=(ins_status==3 & (ehiowner==1|ehiowner==3));

* Any government health insurance;
gen govhi=(ins_status>=4 & ins_status<=6);

* Indicator for any insurance coverage;
gen anyhi=(ins_status>=1 & ins_status<=7);

* Create a variable that indicates that self-reported health status is less than "Excellent";
gen bad_hlth=.;
replace bad_hlth=1 if health!=-1 & health!=1 & health!=.;
replace bad_hlth=0 if health==1;

* Create labor force variable.
1 -> have a job
2 -> looking for a job, but has no job
3 -> not in labor force;

gen labor=0;
replace labor=1 if (rmesr>=1 & rmesr<=5); 
replace labor=2 if (rmesr>=6 & rmesr<=7); 
replace labor=3 if (rmesr==8); 

* hours of work per week;
* a value of -1 indicates that hours vary.;

gen hours=.;
replace hours=-1 if (ejbhrs1==-8 | ejbhrs2==-8);
replace hours=0 if ejbhrs1==-1;
replace hours=ejbhrs1 if (ejbhrs1>0 & ejbhrs2==-1);
replace hours=ejbhrs1+ejbhrs2 if (ejbhrs1>0 & ejbhrs2>0);

* Indicator for full-time workers ;
* Those who reported that their hours vary are coded as part time unless
* they reported 30+ hours for either ejbhrs1 or ejvhrs2 variable.;
* we assign full-time status only if a worker had a job all month;

* Indicator for having a job all month.;
gen joball=0 if rmesr~=-1;
replace joball=1 if rmesr==1 | rmesr==2;
replace joball=. if rmesr>8;

gen ft=0 if hours~=.;
replace ft=1 if hours>=30 & hours~=. & joball==1;
replace ft=1 if ejbhrs1>=30 & ejbhrs2==-8 & joball==1;
replace ft=1 if ejbhrs1==-8 & ejbhrs2>=30 & joball==1;

* indicator for being a student;
gen student=(eenrlm==1); 

* indicator for being a full-time student;
gen ft_student=(renroll==1);

* education variables;
gen hsdo=(eeducate>=31 & eeducate<=38); * less than high school;
gen hsg=(eeducate==39); * high school graduate;
gen somcol =(eeducate>=40 & eeducate<=43); *less than Bachelor's degree;
gen colgrd=(eeducate==44); * Bachelor's degree;

sort suid pnum_spouse year month;
save depcov_dataset, replace;

*******************************************************;
* create variable that indicate changes in employment *;
*******************************************************;

tsset groupid wave;
sort groupid wave;

*The following two lines yield the same results.;

gen empnum_lag1=l.empnum;

* an indicator for changing employers or job status;
gen job_ease=.;
replace job_ease=1 if empnum_lag1!=empnum & empnum_lag1!=. & empnum_lag1!=-1 ; * empnum=-1 means that not in universe (no job);
replace job_ease=0 if empnum_lag1==empnum & job_ease==.;
replace job_ease=0 if empnum_lag1==-1 & job_ease==.;
* note: job_ease==. only when empnum_lag1==.;

gen emp2diffemp=.;
replace emp2diffemp=1 if empnum!=empnum_lag1 & empnum_lag1!=. & empnum_lag1!=-1 & empnum!=-1;
replace emp2diffemp=0 if empnum_lag1!=. & emp2diffemp==.;

drop empnum empnum_lag1;

save depcov_dataset, replace;

***** Assign ins status of a spouse. *****;
keep suid pnum year month ins_status;
rename pnum pnum_spouse;
rename ins_status ins_status_spouse;
sort suid pnum_spouse year month;

merge 1:m suid pnum_spouse year month using depcov_dataset;
tab _merge;
tab mar _merge;
drop if _merge==1;
drop _merge;

* Obtain an indicator for employer provided health insurance by parents' plan.;
gen emphi_dep=0;
replace emphi_dep=1 if mar==0 & ins_status==2;  *unmarried;
replace emphi_dep=1 if mar==1 & ins_status==2 & cov_other==1;  *married but obtain ins from outside HH;
replace emphi_dep=1 if mar==1 & ins_status==2 & ins_status_spouse!=1;  *married but a spouse does not have ESI in own name.;

*!!!!!!!!!!!!!!!!!!!!!!!!ADDED on 02/02/2013 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!;
* Obtain an indicator for employer provided health insurance by spouse's plan.;
gen emphi_dep_spous=(emphi_dep==0 & ins_status==2);
*!!!!!!!!!!!!!!!!!!!!!!!!ADDED on 02/02/2013 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!;

***** Assign parent's ESI status. *****;

* Even if the young adult does not live in the same household with their parent in the current period, we can match the young adult with their parents if they live in the same household in other periods. To do so, we use lags and leads of parent's ID variable.;

* before adding parental id from different periods, save the original parental id variables.;
gen pnum_dad_original=pnum_dad;
gen pnum_mom_original=pnum_mom;

tsset groupid wave;

* obtain the lags of parent's ID number;
gen lagdad1=l.pnum_dad;
gen lagdad2=l2.pnum_dad;
gen lagdad3=l3.pnum_dad;
gen lagdad4=l4.pnum_dad;
gen lagdad5=l5.pnum_dad;
gen lagdad6=l6.pnum_dad;
gen lagdad7=l7.pnum_dad;
gen lagdad8=l8.pnum_dad;
gen lagdad9=l9.pnum_dad;
gen lagmom1=l.pnum_mom;
gen lagmom2=l2.pnum_mom;
gen lagmom3=l3.pnum_mom;
gen lagmom4=l4.pnum_mom;
gen lagmom5=l5.pnum_mom;
gen lagmom6=l6.pnum_mom;
gen lagmom7=l7.pnum_mom;
gen lagmom8=l8.pnum_mom;
gen lagmom9=l9.pnum_mom;

* obtain the leads of parent's ID number;
gen leaddad1=f.pnum_dad;
gen leaddad2=f2.pnum_dad;
gen leaddad3=f3.pnum_dad;
gen leaddad4=f4.pnum_dad;
gen leaddad5=f5.pnum_dad;
gen leaddad6=f6.pnum_dad;
gen leaddad7=f7.pnum_dad;
gen leaddad8=f8.pnum_dad;
gen leaddad9=f9.pnum_dad;
gen leadmom1=f.pnum_mom;
gen leadmom2=f2.pnum_mom;
gen leadmom3=f3.pnum_mom;
gen leadmom4=f4.pnum_mom;
gen leadmom5=f5.pnum_mom;
gen leadmom6=f6.pnum_mom;
gen leadmom7=f7.pnum_mom;
gen leadmom8=f8.pnum_mom;
gen leadmom9=f9.pnum_mom;

forvalues k = 1/9{;
  replace pnum_dad=lagdad`k' if pnum_dad==9999 & lagdad`k'!=9999 & lagdad`k'!=.;
  replace pnum_mom=lagmom`k' if pnum_mom==9999 & lagmom`k'!=9999 & lagmom`k'!=.;
  replace pnum_dad=leaddad`k' if pnum_dad==9999 & leaddad`k'!=9999 & leaddad`k'!=.;
  replace pnum_mom=leadmom`k' if pnum_mom==9999 & leadmom`k'!=9999 & leadmom`k'!=.;
 };


sum;

sort suid pnum_dad_original year month;
save depcov_dataset, replace;


* Merge parent's information.;

* create an indicator for living with parents.;

* Obtain Dad's information *;
keep suid pnum year month adid;
rename pnum pnum_dad_original;
rename adid adid_dad;
sort suid pnum_dad_original year month;

merge 1:m suid pnum_dad_original year month using depcov_dataset;
drop if _merge==1;
drop _merge;

sort suid pnum_mom_original year month;
save depcov_dataset, replace;

* Obtain Mom's information *;
keep suid pnum year month adid;
rename pnum pnum_mom_original;
rename adid adid_mom;
sort suid pnum_mom_original year month;

merge 1:m suid pnum_mom_original year month using depcov_dataset;
drop if _merge==1;
drop _merge;

** Create a variable that indicates living with thier parents.;
gen live_wparent=.;
replace live_wparent=1 if adid==adid_dad;
replace live_wparent=1 if adid==adid_mom & live_wparent==.;
replace live_wparent=0 if adid_mom==. & live_wparent==.;
replace live_wparent=0 if adid_dad==. & live_wparent==.;
drop adid_mom adid_dad pnum_dad_original pnum_mom_original;

sort suid pnum_dad year month;
save depcov_dataset, replace;

* Dad *;
keep suid pnum year month emphi;
rename pnum pnum_dad;
rename emphi emphi_dad; 
sort suid pnum_dad year month;

merge 1:m suid pnum_dad year month using depcov_dataset;
drop if _merge==1;
drop _merge;

sort suid pnum_mom year month;
save depcov_dataset, replace;

* Mom *;
keep suid pnum year month emphi;
rename pnum pnum_mom;
rename emphi emphi_mom;
sort suid pnum_mom year month;

merge 1:m suid pnum_mom year month using depcov_dataset;
drop if _merge==1;
drop _merge;

** Create a variable that indicates one of the parent has EHI own coverage.;
gen emphi_parent=.;
replace emphi_parent=1 if (emphi_dad==1|emphi_mom==1);
replace emphi_parent=0 if emphi_parent==. & emphi_dad==0;
replace emphi_parent=0 if emphi_parent==. & emphi_mom==0;
* definition is modified later in this file.;

** Create a variable that indicates that parents' information is available in SIPP.;

gen info_parent= (emphi_dad!=.|emphi_mom!=.);

sort suid pnum_dad year month;
save depcov_dataset, replace;

*** Create a variable that indicates a number of dependent covered by the parent’s ESI. ***;

* Count the number of Dad's dependents;
keep suid pnum_dad year month emphi_dep;
drop if pnum_dad==9999;
collapse (sum) dad_emphi_dep=emphi_dep, by (suid pnum_dad year month);
sort suid pnum_dad year month;
sum;

merge 1:m suid pnum_dad year month using depcov_dataset;
drop if _merge==1;
drop _merge;

sort suid pnum_mom year month;
save depcov_dataset, replace;

* Count the number of Mom's dependents;
keep suid pnum_mom year month emphi_dep;
drop if pnum_mom==9999;
collapse (sum) mom_emphi_dep=emphi_dep, by (suid pnum_mom year month);
sort suid pnum_mom year month;
sum;

merge 1:m suid pnum_mom year month using depcov_dataset;
drop if _merge==1;
drop _merge;

* Create a variable that indicates a number of dependents covered by parent's ESI.;
gen num_emphi_dep=.;
replace num_emphi_dep=dad_emphi_dep+mom_emphi_dep if mom_emphi_dep!=. & dad_emphi_dep!=.;
replace num_emphi_dep= dad_emphi_dep if num_emphi_dep==.;
replace num_emphi_dep= mom_emphi_dep if num_emphi_dep==.;

* Change the definition of emphi_parent. Include the following case: At least one member of the HH is covered by parent’s ESI as a dependent (If the parent does not live in the same HH, parent's ESI status cannot be captured by the codes used above.).;

replace emphi_parent=1 if emphi_parent==0 & num_emphi_dep>0 & num_emphi_dep!=.;
replace emphi_parent=1 if emphi_parent==. & num_emphi_dep>0 & num_emphi_dep!=.;

sort suid pnum;
save depcov_dataset, replace;


* Assign an indicator for: at least one dependent other than the young adult was covered by the parent’s ESI in Nov 2009 -Feb 2010 and the young adult was not covered by parents' ESI: whether a parent has ESI in Nov 2009 -Feb 2010, whether a person has emphi_dep. This variable is used in marginal cost analysis (Table A5); 

keep suid pnum year month num_emphi_dep emphi_dep emphi_parent anyhi;
keep if (year==2009 & month>=11) |(year==2010 & month<=2);

gen other_dep_pre=0; 
replace other_dep_pre=1 if emphi_dep==0 & num_emphi_dep>0 & num_emphi_dep!=.;

rename emphi_dep emphi_dep_pre;
rename emphi_parent emphi_p_pre;
rename anyhi anyhi_pre;

drop year month num_emphi_dep;
sort suid pnum;

merge 1:m suid pnum using depcov_dataset;
drop if _merge==1;
drop _merge;

drop srefmon ems ehimth ehemply rchampm ecdmth ecrmth eorigin erace ehiowner esex suid pnum_spouse ins_status ins_status_spouse wave famnum famyinc pnum cov_other emphi_dep_all health rmesr eeducate eenrlm renroll famyincy my_fratio pnum_dad pnum_mom emphi_mom emphi_dad dad_emphi_dep mom_emphi_dep lag* lead*;


* Limit population;
keep if age<=29 & age>=16;
drop if age==26;   * we will not include age==26 population.;

* save the dataset.;
save depcov_dataset, replace;

sum;



*Delete wave files;
forval j = 1/10{;
erase sippl08puw`j'.dta;
clear;
};

erase sippp08putm4.dta;
clear all;
exit;


clear;
log close;
exit;


