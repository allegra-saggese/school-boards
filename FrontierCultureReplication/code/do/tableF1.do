
cap log c
log using "$outdirapp/tableF1.txt", replace text
			
*NB: add standard errors and p-values in tex file by hand			
			
*panels (a) and (b)
use "$dir/data/names1940", clear
	
	cap g yrEnt = lunsetyr500kNoIs_110
	replace yrEnt = 0 if yrEnt==.
				
	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
	
	qui xi: reg shkpt10d $frontierX $otherX i.statea if sampE_W1790E1890==1
	keep if e(sample)
	
	foreach dep of varlist shkpt10d shmkpt10d {
	
		su `dep'
		replace `dep' = (`dep' - r(mean))/r(sd)
		
		eststo clear

		eststo : xi: reg `dep' $frontierX $otherX i.statea, $SE  

		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(100) spatial
				
		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(200) spatial
				
		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(300) spatial

		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(500) spatial
				
		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(1000) spatial

		eststo : xi: reghdfe `dep' $frontierX $otherX, cluster(km_grid_cel_code yrEnt) absorb(statea)
				
		eststo : xi: reg `dep' $frontierX $otherX i.statea, cluster(statea)
		boottest $frontierX, nograph seed(12345) reps(9999) boottype(wild)			

		#d ;
		esttab using "$outdirapp/tableF1-`dep'.tex", 
			replace noobs nomtitle nodepvar plain fragment label 
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX , relax)
			keep( $frontierX , relax) varwidth(30)
			$STAR 						
			mlabels(, none) collabels(, none)
			stats(N, fmt(%9.0fc)	labels("Number of Counties"))	
			substitute(_ \_) style(tex) prefoot( & & & & & & & & \\ \hline );		
		#d cr	
		
	}
	
*panels (c) and (d)
use "$dir/data/proptaxvote",clear	
	
	cap g yrEnt = lunsetyr500kNoIs_110
	replace yrEnt = 0 if yrEnt==.

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"

	qui xi: reg avgrep2000to2016 $frontierX $otherX i.statea if sampE_W1790E1890==1
	keep if e(sample)
	
	foreach dep of varlist propertytaxrate2010 avgrep2000to2016  {
	
		su `dep'
		
		eststo clear

		eststo : xi: reg `dep' $frontierX $otherX i.statea, $SE  

		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(100) spatial
				
		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(200) spatial
				
		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(300) spatial

		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(500) spatial
				
		eststo : xi: acreg `dep' $frontierX $otherX i.statea, longitude(lon) latitude(lat) dist(1000) spatial

		eststo : xi: reghdfe `dep' $frontierX $otherX, cluster(km_grid_cel_code yrEnt) absorb(statea)
				
		eststo : xi: reg `dep' $frontierX $otherX i.statea, cluster(statea)
		boottest $frontierX, nograph seed(12345) reps(9999) boottype(wild)			

		#d ;
		esttab using "$outdirapp/tableF1-`dep'.tex", 
			replace noobs nomtitle nodepvar plain fragment label 
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX , relax)
			keep( $frontierX , relax) varwidth(30)
			$STAR 						
			mlabels(, none) collabels(, none)
			stats(N, fmt(%9.0fc)	labels("Number of Counties"))	
			substitute(_ \_) style(tex) prefoot( & & & & & & & & \\ \hline );		
		#d cr	
		
	}			
				
log c
