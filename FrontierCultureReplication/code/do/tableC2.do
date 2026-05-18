
eststo clear

use "$dir/data/cces.dta", clear 

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
	
	lab var totYear_pdLess6_1790to1890 "total low density experience"

	keep if sampE_W1790E1890==1
			
	foreach dep of varlist repealACA opposesIncreaseMinWage opposesBanAssaultRifle opposeEPAregulateCO2 {

		xi: reg `dep' $frontierX $FEs $indX $otherX, $SE
		xi: reg `dep' $frontierX $FEs if e(sample), $SE
		global baseR2 = e(r2)
		global baseTFE = _b[$frontierX]
		
		eststo : xi: reg `dep' $frontierX $indX $otherX $FEs, $SE 
		qui distinct fips if e(sample)
		estadd scalar numC = r(ndistinct)		
		estadd ysumm			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
	}
				
	#d ;
	esttab using "$outdirapp/tableC2.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( $frontierX , relax)
		keep( $frontierX , relax) varwidth(30)
		$STAR					
		mlabels(, none) collabels(, none) 
		stats(ostDelta N numC ymean r2, fmt(%9.2f %9.0fc %9.0fc %9.2f %9.2f )
		labels("Oster $\delta$ for $\beta=0$" "Number of Individuals" "Number of Counties" "Mean of Dependent Variable" "R\$^2\$"))
		substitute(_ \_) style(tex) prefoot( & & & & \\ \hline);		
	#d cr	
