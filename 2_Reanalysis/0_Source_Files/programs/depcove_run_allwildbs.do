clear all
capture log close
#delimit ;
set more 1;
set mem 9g;
/*

CHANGE PATH(BELOW) TO LOCATION OF WILD CLUSTER BOOTSTRAP FOLDER;
*/

cd "E:\Projects\CA\DependentCoverage\asako\test_replication_package\AEJEPcode\data\wild_cluster_bs";
local prog "E:\Projects\CA\DependentCoverage\asako\test_replication_package\AEJEPcode\data\wild_cluster_bs";
log using `prog'\wild_cluster_bs.log,replace;



*Run all wild bootstrap programs;

do `prog'\wild_bs_aggregate_anyhi_mar10.do;
do `prog'\wild_bs_aggregate_anyhi_oct10.do;
do `prog'\wild_bs_aggregate_emphi_dep_mar10.do;
do `prog'\wild_bs_aggregate_emphi_dep_oct10.do;
do `prog'\wild_bs_aggregate_indiv_mar10.do;
do `prog'\wild_bs_aggregate_indiv_oct10.do;
do `prog'\wild_bs_aggregate_emphi_mar10.do;
do `prog'\wild_bs_aggregate_emphi_oct10.do;
do `prog'\wild_bs_aggregate_govhi_mar10.do;
do `prog'\wild_bs_aggregate_govhi_oct10.do;


clear;
exit;
