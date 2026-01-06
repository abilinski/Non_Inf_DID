/*
This program converts all the raw data of the 2008 SIPP panel waves 1-10 and topical wave
to stata files
*/


capture log close
#delimit ;
set more 1;
set mem 9g;

/*
CHANGE DIRECTORIES BELOW;
*/

***This directory has all the raw data***;
global indir "E:\Projects\CA\DependentCoverage\asako\test_replication_package\AEJEPcode\data";

****This directory has the .dct programs***;
global inprog "E:\Projects\CA\DependentCoverage\asako\test_replication_package\AEJEPcode\programs\readin";



*Convert all .dat files to STATA data files;
forval j = 1/10{;
do ${inprog}\sippl08puw`j'.do;
clear;
};

do ${inprog}\sippp08putm4.do;
clear all;
exit;
