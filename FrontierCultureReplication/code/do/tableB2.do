
global frontierX tye_tfe890_500kNI_100_l6
lab var $frontierX "total frontier experience"

eststo clear

	keep if year==1990
				
	xi: reg cooperateNOTselfRely $frontierX $indX if sampE_W1790E1890==1, $SE
	keep if e(sample)
 
	xi: reg cooperateNOTselfRely $frontierX $indX, $SE
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	qui distinct year if e(sample)
	estadd scalar numY = r(ndistinct)
	eststo a2 
	estadd ysumm			
	local maxR = min(e(r2)*1.3, 1)
	psacalc delta ${frontierX} , rmax(`maxR') beta(0) 
	estadd scalar ostDelta = r(delta): a2			
	
	xi: reg cooperateNOTselfRely $frontierX $indX $FEd, $SE
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	qui distinct year if e(sample)
	estadd scalar numY = r(ndistinct)
	eststo a3
	estadd ysumm			
	local maxR = min(e(r2)*1.3, 1)
	psacalc delta ${frontierX} , rmax(`maxR') beta(0) 
	estadd scalar ostDelta = r(delta): a3		
	
	xi: reg cooperateNOTselfRely $frontierX $indX $FEs, $SE
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	qui distinct year if e(sample)
	estadd scalar numY = r(ndistinct)
	eststo a4
	estadd ysumm			
	local maxR = min(e(r2)*1.3, 1)
	psacalc delta ${frontierX} , rmax(`maxR') beta(0) 
	estadd scalar ostDelta = r(delta): a4	
				
	xi: reg cooperateNOTselfRely $frontierX $indX $otherX $FEd, $SE
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	qui distinct year if e(sample)
	estadd scalar numY = r(ndistinct)
	eststo a5
	estadd ysumm			
	local maxR = min(e(r2)*1.3, 1)
	psacalc delta ${frontierX} , rmax(`maxR') beta(0) 
	estadd scalar ostDelta = r(delta): a5	

#d ;
esttab using "$outdirapp/tableB2.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
	order( $frontierX , relax)
	keep( $frontierX , relax) varwidth(30)
	$STAR					
	mlabels(, none) collabels(, none) prehead( & & & & \\ )
	substitute(_ \_) style(tex) prefoot( & & & & \\ \hline)	
	stats(ostDelta N numC ymean r2, fmt(%9.2f %9.0fc %9.0fc %9.3f %9.2f )
	labels("Oster $\delta$ for $\beta=0$" "Number of Individuals" "Number of Counties" "Mean of Dependent Variable" "R\$^2\$"));
#d cr
