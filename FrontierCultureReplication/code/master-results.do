
*NOTE: all code run with restricted-access data has been commented out below (see the README.pdf for details)

global root "D:\Dropbox\BigPushB" // please change this to wherever the folder is located on your computer
global dir "${root}/FrontierCultureReplication"
cd "$dir"

cap mkdir "$dir/output"
cap mkdir "$dir/output/tabfig"
global outdir "$dir/output/tabfig"

cap mkdir "$dir/output/tabfigapp"
global outdirapp "$dir/output/tabfigapp"

*global nberdir "" 
*global outdirnber "" 

*cap mkdir "$nberdir/code"
*cap mkdir "$outdirnber"

set seed 12345

*----------------------------------*
* SETUP
*----------------------------------*

do "$dir/code/do/setup"

*if you want stars, reverse the globals below

global STAR starlevels(* 0.1 ** 0.05 *** 0.01) 
global STARcell star

global STAR
global STARcell

set scheme s1mono

global SE cluster(km_grid_cel_code)
global FEs i.year i.statea
global FEd i.year D2MA D3ENC D4WNC D5SA D6ESC D7WSC D8MT D9PC   
global indX age ageSQ male racewhit raceblac
global otherX log_area_2010 lat lon temp_mean rain_mean elev_mean d_coa d_riv d_lak ave_gyi 
global otherXnewPre tri_ave ppt_risk d_port d_mrdspre1890 d_batt shslav1860 wsexrat1890 fb_shr1890 fbscotirel_shr1890 bplfrac_1890 yearswithRRbef1890 shempmanu1890 

cap program drop tabAdd
program define tabAdd
	distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)	
	estadd ysumm	
end		

************************************************************************************
* MAIN PAPER
************************************************************************************

*-----------------------------------*
* SECTION 3
*-----------------------------------*

use "$dir/data/historicalDescriptive", clear
	do "$dir/code/do/figure4"
	do "$dir/code/do/table1"
	
*-----------------------------------*
* SECTION 4
*-----------------------------------*

use "$dir/data/historicalElection", clear
	do "$dir/code/do/figure5"

use "$dir/data/names1940", clear
	do "$dir/code/do/table2"
	
*do "$dir/code/table3"

do "$dir/code/do/table4"

use "$dir/data/cces", clear
	do "$dir/code/do/table5"

*-----------------------------------*
* SECTION 5
*-----------------------------------*

*use "$nberdir/selectionreturns1850to1880", clear
*	do "$dir/code/do/table6"
*	do "$dir/code/do/table9"

*use "$nberdir/moverONE", clear
*	do "$dir/code/do/table7figure6"

*use "$nberdir/moverTWO", clear
*	do "$dir/code/do/table8figure7"
	
************************************************************************************
* ONLINE APPENDIX
************************************************************************************

use "$dir/data/historicalDescriptive", clear
	do "$dir/code/do/tableB1"

use "$dir/data/anes",clear
	do "$dir/code/do/tableB2"

use "$dir/data/names1940", clear
	do "$dir/code/do/tableB3"
	
*use "$nberdir/names_1940_individual", clear
*	do "$dir/code/do/tableB4"

use "$dir/data/names1910_40", clear
	do "$dir/code/do/tableB5"

do "$dir/code/do/tableB6" 

do "$dir/code/do/tableB7"

use "$dir/data/congSpeech", clear
	do "$dir/code/do/tableC1"

do "$dir/code/do/tableC2"

do "$dir/code/do/tableD1andD2"

do "$dir/code/do/figuresD1toD4"

*use "$nberdir/moverONE", clear
*	do "$dir/code/do/figureE1"

*use "$nberdir/moverTWO", clear
*	do "$dir/code/do/tableE1figureE2"
		
************************************************************************************
* ADDITIONAL MATERIAL APPENDIX
************************************************************************************

do "$dir/code/do/tableF1"

use "$dir/data/historicalDescriptive", clear
	do "$dir/code/do/figureG1"
	do "$dir/code/do/figureG2"
	do "$dir/code/do/figureG3"
	do "$dir/code/do/figureG4"
	do "$dir/code/do/tableG1"
	
*use "$nberdir/selectionLinked", clear
*	do "$dir/code/do/tableI1"

use "$dir/data/disease1880", clear
	do "$dir/code/do/tableJ4"

do "$dir/code/do/tableJ2"
