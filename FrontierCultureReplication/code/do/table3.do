
eststo clear
			
*(1)
use "$dir/data/anes", clear
	keep if sampE_W1790E1890==1
	xi: reg cutSpendPoor $frontierX $otherX $indX $FEs, $SE
	xi: reg cutSpendPoor $frontierX $FEs if e(sample), $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	xi: reg cutSpendPoor $frontierX $indX $otherX $FEs, $SE 
	eststo b1 
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): b1				

*(2)	
use "$dir/data/cces", clear
	keep if sampE_W1790E1890==1
	xi: reg stateDecreaseWelfare $frontierX $FEs $otherX $indX, $SE
	xi: reg stateDecreaseWelfare $frontierX $FEs if e(sample), $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	xi: reg stateDecreaseWelfare $frontierX $indX $otherX $FEs, $SE 	
	eststo b2 
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): b2

*(3)
use "$dir/data/gss", clear
	keep if sampE_W1790E1890==1
	xi: reg zgovRedIncDif1to7 $frontierX $indX $otherX $FEd, $SE
	xi: reg zgovRedIncDif1to7 $frontierX $FEd if e(sample), $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	xi: reg zgovRedIncDif1to7 $frontierX $indX $otherX $FEd, $SE 		
	eststo b3 
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): b3

*(4)
	use "$dir/data/cces", clear
	keep if sampE_W1790E1890==1
	xi: reg cutDomesticSpend $frontierX $FEs $otherX $indX, $SE
	xi: reg cutDomesticSpend $frontierX $FEs if e(sample), $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	xi: reg cutDomesticSpend $frontierX $indX $otherX $FEs, $SE 	
	eststo b4
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): b4

*(5)
use "$dir/data/gss", clear
	keep if sampE_W1790E1890==1
	xi: reg zindexTooMuchSpend $frontierX $indX $otherX $FEd, $SE
	xi: reg zindexTooMuchSpend $frontierX $FEd if e(sample), $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	xi: reg zindexTooMuchSpend $frontierX $indX $otherX $FEd, $SE 		
	eststo b5
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): b5

*(6) 
use "$dir/data/proptaxvote", clear
	keep if sampE_W1790E1890==1
	xi: reg propertytaxrate2010 $frontierX i.statea $otherX, $SE
	xi: reg propertytaxrate2010 $frontierX i.statea if e(sample), $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	xi: reg propertytaxrate2010 $frontierX $otherX i.statea, $SE 		
	eststo b6 
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): b6

*(7)
	xi: reg avgrep2000to2016 $frontierX i.statea $otherX, $SE
	xi: reg avgrep2000to2016 $frontierX i.statea if e(sample), $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	xi: reg avgrep2000to2016 $frontierX $otherX i.statea, $SE 		
	eststo b7
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): b7

lab var $frontierX "total frontier experience"

#d ;
esttab b* using "$outdir/table3.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
	order( $frontierX , relax)
	keep( $frontierX , relax) varwidth(30)
	$STAR						
	mlabels(, none) collabels(, none) 
	stats(ostDelta ymean N numC r2, fmt(%9.2f %9.2f %9.0fc %9.0fc %9.2f)
	labels("Oster $\delta$ for $\beta=0$" "Mean of Dependent Variable" "Number of Individuals" "Number of Counties" "R\$^2\$"))
	substitute(_ \_) style(tex) prefoot( & & & & & & & \\ \hline );		
#d cr	
