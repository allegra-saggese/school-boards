	  
	eststo clear

	*names
	use "$dir/data/names1940", clear

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
	
	qui xi: reg shkpt10d $frontierX $otherX i.statea if sampE_W1790E1890==1
	keep if e(sample)
	
	foreach dep in shkpt10d shmkpt10d {

		g _`dep' = `dep'
	
		su `dep'
		replace `dep' = (`dep' - r(mean))/r(sd)

		eststo a_`dep' : xi: reg `dep' $frontierX $otherX i.statea, $SE  
		estadd ysumm		
	
		drop `dep'
		ren _`dep' `dep'
		
		su `dep'
		replace `dep' = (`dep' - r(mean))/r(sd)

		eststo b_`dep' : xi: reg `dep' $frontierX $otherX i.statea if ${frontierX}>0, $SE  
		estadd ysumm	
		
	}
			 
	*mu cces			
	use "$dir/data/cces",clear

	keep if sampE_W1790E1890==1

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"

	eststo a_muCCES : xi: reg muCCES $frontierX $FEs $indX $otherX, $SE  
	estadd ysumm		

	eststo b_muCCES : xi: reg muCCES $frontierX $FEs $indX $otherX if ${frontierX}>0, $SE  
	estadd ysumm					
			 
	*property tax and republican vote
	use "$dir/data/proptaxvote",clear

	keep if sampE_W1790E1890==1

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
	
	foreach dep in propertytaxrate2010 avgrep2000to2016 {

		eststo a_`dep' : xi: reg `dep' $frontierX $otherX i.statea, $SE  
		estadd ysumm		

		eststo b_`dep' : xi: reg `dep' $frontierX $otherX i.statea if ${frontierX}>0, $SE  
		estadd ysumm	
		
	}

	#d ;
	esttab a_* using "$outdirapp/tableJ1a.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( $frontierX , relax)
		keep( $frontierX , relax) varwidth(30)
		$STAR
		mlabels(, none) collabels(, none) 
		stats(N ymean, fmt(%9.0fc %9.2f)
		labels("Number of Observations" "Mean of Dependent Variable"))
		substitute(_ \_) style(tex) prefoot( & & & & & \\ \hline );		
	#d cr	

	#d ;
	esttab b_* using "$outdirapp/tableJ1b.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( $frontierX , relax)
		keep( $frontierX , relax) varwidth(30)
		$STAR
		mlabels(, none) collabels(, none) 
		stats(N ymean, fmt(%9.0fc %9.2f)
		labels("Number of Observations" "Mean of Dependent Variable"))
		substitute(_ \_) style(tex) prefoot( & & & & & \\ \hline );		
	#d cr	

