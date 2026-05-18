
eststo clear

global frontXint frontXwhite frontXblack frontXother
global race racewhite raceblack 

keep if sampE_W1790E1890==1			

foreach dep of varlist stateDecreaseWelfare cutDomesticSpend repealACA opposesIncreaseMinWage opposesBanAssaultRifle opposeEPAregulateCO2 {
	
	eststo : xi: reg `dep' $frontXint $indX $otherX $FEs, $SE 			

	test frontXwhite = frontXblack
	estadd scalar pval = r(p)
	
	qui distinct fips if e(sample)
	estadd scalar numC = r(ndistinct)
	qui su racewhite if e(sample)
	estadd scalar shrWhite = r(mean)
	qui su raceother if e(sample)
	estadd scalar shrOther = r(mean)
	qui su raceblack if e(sample)
	estadd scalar shrBlack = r(mean)
	qui su `dep' if racewhite==1 & e(sample)
	estadd scalar muWhite = r(mean)

}		  			
#d ;
esttab using "$outdir/table5.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
	order( $frontXint $race, relax)
	keep( $frontXint $race, relax) varwidth(30)
	$STAR						
	mlabels(, none) collabels(, none) 
	stats(N numC pval muWhite shrWhite shrOther shrBlack, fmt(%9.0fc %9.0fc %9.3f %9.2f %9.2f %9.2f %9.2f)
	labels("Number of Individuals" "Number of Counties" "TFE(black)=TFE(white), p-value" "Mean of Dependent Variable, Whites" 
			"Share White Respondents" "Share Black Respondents" "Share Other Respondents"))
	substitute(_ \_) style(tex) prefoot( & & & & & & \\ \hline );		
#d cr	
