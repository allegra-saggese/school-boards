
cap program drop labTFE
program define labTFE
				 
	cap lab var tye_tfe890_500kNI_100_l6 "TFE: 100 km, $\leq 6$/mi$^2$, no inner or outer islands"
	cap lab var tye_tfe890_500kNI_50_l6 "TFE: 50 km, $\leq 6$/mi$^2$, no inner or outer islands"
	cap lab var tye_tfe890_500kNI_100_l18 "TFE: 100 km, $\leq 18$/mi$^2$, no inner island lines"
	cap lab var tye_tfe890_500kNI_50_l18 "TFE: 50 km, $\leq 18$/mi$^2$, no inner island lines"
	cap lab var tye_tfe890_500kNI_100_2to6 "TFE: 100 km, 2-6/mi$^2$, no inner island lines"
	cap lab var tye_tfe890_500kNI_50_2to6 "TFE: 50 km, 2-6/mi$^2$, no inner island lines"
	cap lab var tye_tfe890_500kNI_100_0 "TFE: 100 km, no density restriction, no inner island lines"
	cap lab var tye_tfe890_500kNI_50_0 "TFE: 50 km, no density restriction, no inner island lines"
	cap lab var tye_tfe890_500k_100_l6 "TFE: 100 km, $\leq 6$/mi$^2$, including inner island lines"
	cap lab var tye_tfe890_500k_50_l6 "TFE: 50 km, $\leq 6$/mi$^2$, including inner island lines"
	cap lab var tye_tfe890_ONE_100_l6 "TFE: 100 km, $\leq 6$/mi$^2$, main single contour line"
	cap lab var tye_tfe890_ONE_50_l6 "TFE: 50 km, $\leq 6$/mi$^2$, main single contour line"
	cap lab var tye_tfe890_500kNoIs_100_l6 "TFE: 50 km, $\leq 6$/mi$^2$, no inner or outer island lines"
	cap lab var tye_tfe890_500kNoIs_50_l6 "TFE: 50 km, $\leq 6$/mi$^2$, no inner or outer island lines"

end



foreach FRONT in tye_tfe890_500kNI_100_l6 tye_tfe890_500kNI_50_l6 tye_tfe890_500kNI_100_l18 tye_tfe890_500kNI_50_l18 tye_tfe890_500kNI_100_2to6 ///
				tye_tfe890_500kNI_50_2to6 tye_tfe890_500kNI_100_0 tye_tfe890_500kNI_50_0 tye_tfe890_500k_100_l6 tye_tfe890_500k_50_l6 ///
				tye_tfe890_ONE_100_l6 tye_tfe890_ONE_50_l6 tye_tfe890_500kNoIs_100_l6 tye_tfe890_500kNoIs_50_l6 { 

	eststo clear
	
	global frontierX `FRONT' 

	*names
	use "$dir/data/names1940", clear
	
	labTFE

	qui xi: reg shkpt10d $otherX i.statea if sampE_W1790E1890==1
	keep if e(sample)

	foreach v of varlist shkpt10d shmkpt10d {
		qui su `v'
		qui replace `v' = (`v' - r(mean))/r(sd)
	}
	foreach dep of varlist shkpt10d shmkpt10d { 
		eststo: xi: reg `dep' $frontierX $otherX i.statea , $SE  
	}	
	
	*cces
	use "$dir/data/cces.dta", clear 

	keep if sampE_W1790E1890==1 
	
	labTFE

	eststo: xi: reg muCCES $frontierX $otherX $indX i.statea i.year, $SE  
		estadd ysumm				

	
	*property tax and republican vote
	use "$dir/data/proptaxvote",clear

	keep if sampE_W1790E1890==1 

	labTFE
	
	foreach dep of varlist propertytaxrate2010 avgrep2000to2016 { 
		eststo : xi: reg `dep' $frontierX $otherX i.statea, $SE 		
	}
	
	#d ;
	esttab using "$outdirapp/tableJ2.tex", 
		append noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( $frontierX , relax)
		keep( $frontierX , relax) varwidth(30)
		$STAR 						
		mlabels(, none) collabels(, none) 
		substitute(_ \_) style(tex) prefoot( & & & & & \\);		
	#d cr	
   eststo clear
}	
