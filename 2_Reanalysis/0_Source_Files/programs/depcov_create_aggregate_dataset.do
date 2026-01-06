* This program produces aggregate dataset that is used to produce results in Table A6. DD Results using Aggregated Quarterly Data and Wild Cluster-Bootstrap Percentile-t Procedure .;

#delimit ;
clear all;
capture log close;

/*
CHANGE PATH(BELOW) TO LOCATION OF DATA;
*/
cd "E:\Projects\CA\DependentCoverage\asako\test_replication_package\AEJEPcode\data";
set more 1;
set mem 900m;
log using depcov_create_aggregate_dataset.log , replace ;

use depcov_dataset;

gen ctrl = ((age>=16&age<=18) | (age>=27 & age<=29));

gen qtr=0;
replace qtr=1 if month>=1 & month<=3;
replace qtr=2 if month>=4 & month<=6;
replace qtr=3 if month>=7 & month<=9;
replace qtr=4 if month>=10 & month<=12;

tempfile data_for_aggregate;
save `data_for_aggregate', replace;

* obtain the rate of coverage by control/treatment group.;

foreach instype in anyhi emphi_dep emphi indiv govhi{;

  use `data_for_aggregate';

  collapse (mean) `instype' [weight=p_weight], by(year qtr ctrl);
  sort ctrl year qtr;
  tempfile `instype';
  save ``instype'', replace;

};


* merge the rates;
use `anyhi';
sort ctrl year qtr;
merge ctrl year qtr using `emphi_dep' `emphi' `indiv' `govhi';
  drop _merge*;
sort ctrl year qtr;
egen time=group(year qtr); 

gen fedelig = ctrl==0;
gen mar_sep10=(year==2010 & qtr>=2 & qtr<=3);
gen after_oct10=( (year==2010 & qtr>=4) |year>=2011);
gen elig_mar10=mar_sep10*fedelig;
gen elig_oct10=after_oct10*fedelig;

drop year qtr;

save wild_cluster_bs\aggregate_data_wild_bs, replace;

log close;
clear;
exit;
