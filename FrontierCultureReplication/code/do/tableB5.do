
global frontierX tye_tfe890_500kNI_100_l6
lab var $frontierX "total frontier experience"

keep if sampE_W1790E1890==1

*standardizing
foreach year in 1910 1920 1930 1940 {
	foreach dep of varlist shkpt10d shmkpt10d {
		qui xi: reg `dep' $frontierX $otherX i.statea if year==`year' 
		qui su `dep' if e(sample)
		qui replace `dep' = (`dep' - r(mean))/r(sd) if e(sample)
	}
}

foreach dep of varlist shkpt10d shmkpt10d {
	
	eststo clear
	
	foreach year in 1910 1920 1930 1940 {
	
		eststo OLS`year': xi: reg `dep' $frontierX $otherX i.statea if year==`year' , $SE
		local maxR = min(e(r2)*1.3, 1)
		cap psacalc delta $frontierX , rmax(`maxR') beta(0) 
		cap estadd scalar ostDelta = r(delta): OLS`year'
		
	} 
	
	#d ;
	esttab using "$outdirapp/tableB5-`dep'.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( $frontierX , relax)
		keep( $frontierX , relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none) prehead( & & & & \\ )
		substitute(_ \_) style(tex) prefoot( & & & & \\ \hline)	
		stats(N r2, fmt( %9.0fc %9.2f   )
		labels("Number of Counties" "R\$^2\$"  )) ;
	#d cr			
}  
		

