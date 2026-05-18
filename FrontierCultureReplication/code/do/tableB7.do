
*panels (a) and (b)
use "$dir/data/names1940", clear
	
	eststo clear		
			
	foreach samp in sampEW_1890 sampMIDWEST_EW1890 sampSOUTH_EW1890 sampWEST_EW1890 sampEW_pd1950g2 sampMIDWEST_pd1950g2 sampSOUTH_pd1950g2 sampWEST_pd1950g2 {  	 
		
		cap drop front_var
		
		if "`samp'"=="sampEW_1890" | "`samp'"=="sampMIDWEST_EW1890" | "`samp'"=="sampSOUTH_EW1890" | "`samp'"=="sampWEST_EW1890"  {
		   g front_var =  tye_tfe890_500kNI_100_l6
		}
		if "`samp'"=="sampEW_pd1950g2" | "`samp'"=="sampMIDWEST_pd1950g2" | "`samp'"=="sampSOUTH_pd1950g2" | "`samp'"=="sampWEST_pd1950g2"  {
		   g front_var = tye_tfe950_500kNI_100_l6
		}		
		lab var front_var "total frontier experience"
				
		preserve 
		
			qui xi: reg shkpt10d $otherX i.statea if `samp'==1
			keep if e(sample)
			
			foreach v of varlist shkpt10d patmat shmkpt10d {
				qui su `v'
				qui replace `v' = (`v' - r(mean))/r(sd) 
			}
			eststo a_`samp': xi: reg shkpt10d front_var $otherX i.statea, $SE  
			qui distinct fips if e(sample)
			estadd scalar numC = r(ndistinct)
			estadd ysumm	
					
			eststo b_`samp': xi: reg shmkpt10d front_var $otherX i.statea, $SE  
			qui distinct fips if e(sample)
			estadd scalar numC = r(ndistinct)
			estadd ysumm	
					
			eststo c_`samp': xi: reg patmat front_var $otherX i.statea, $SE  
			qui distinct fips if e(sample)
			estadd scalar numC = r(ndistinct)
			estadd ysumm	
			
		restore

	}

	#d ;
	esttab a_* using "$outdirapp/tableB7-shkpt10d.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( front_var, relax)
		keep( front_var , relax) varwidth(30)
		$STAR 						
		mlabels(, none) collabels(, none) coeflabels(front_var "total frontier experience")
		stats(N, fmt(%9.0fc  )
		labels("Number of Counties" ))
		substitute(_ \_) style(tex) prefoot( & & & & & & & & \\ \hline ) postfoot(\hline );	
		
	esttab b_* using "$outdirapp/tableB7-shmkpt10d.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( front_var, relax)
		keep( front_var , relax) varwidth(30)
		$STAR 						
		mlabels(, none) collabels(, none) coeflabels(front_var "total frontier experience")
		stats(N, fmt(%9.0fc  )
		labels("Number of Counties" ))
		substitute(_ \_) style(tex) prefoot( & & & & & & & & \\ \hline ) postfoot(\hline );				

	esttab c_* using "$outdirapp/tableB7-patmat.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( front_var , relax)
		keep( front_var , relax) varwidth(30)
		$STAR 						
		mlabels(, none) collabels(, none)  coeflabels(front_var "total frontier experience")
		stats(N, fmt(%9.0fc  )
		labels("Number of Counties" ))
		substitute(_ \_) style(tex) prefoot( & & & & & & & & \\ \hline ) postfoot(\hline );		
	#d cr

*panel (c)
use "$dir/data/cces.dta", clear 

	eststo clear	
	
	foreach samp in sampEW_1890 sampMIDWEST_EW1890 sampSOUTH_EW1890 sampWEST_EW1890 sampEW_pd1950g2 sampMIDWEST_pd1950g2 sampSOUTH_pd1950g2 sampWEST_pd1950g2 {  	 
		
		cap drop front_var

		if "`samp'"=="sampEW_1890" | "`samp'"=="sampMIDWEST_EW1890" | "`samp'"=="sampSOUTH_EW1890" | "`samp'"=="sampWEST_EW1890"  {
		   g front_var =  tye_tfe890_500kNI_100_l6
		}
		if "`samp'"=="sampEW_pd1950g2" | "`samp'"=="sampMIDWEST_pd1950g2" | "`samp'"=="sampSOUTH_pd1950g2" | "`samp'"=="sampWEST_pd1950g2"  {
		   g front_var = tye_tfe950_500kNI_100_l6
		}		
		lab var front_var "total frontier experience"
		
		eststo : xi: reg muCCES front_var $indX $otherX i.statea i.year if `samp'==1 , $SE  
		qui distinct fips if e(sample)
		estadd scalar numC = r(ndistinct)
		estadd ysumm	
				
	}
	
	#d ;
	esttab using "$outdirapp/tableB7-muCCES.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( front_var, relax)
		keep( front_var , relax) varwidth(30)
		$STAR 						
		mlabels(, none) collabels(, none) coeflabels(front_var "total frontier experience")
		stats(N ymean, fmt(%9.0fc %9.2f)
		labels("Number of Individuals" "Mean of Dependent Variable"))
		substitute(_ \_) style(tex) prefoot( & & & & & & & & \\ \hline ) postfoot(\hline );		
	#d cr
	
*panel (d) and (e)
use "$dir/data/proptaxvote",clear	
		
	eststo clear		
			
	foreach samp in sampEW_1890 sampMIDWEST_EW1890 sampSOUTH_EW1890 sampWEST_EW1890 sampEW_pd1950g2 sampMIDWEST_pd1950g2 sampSOUTH_pd1950g2 sampWEST_pd1950g2 {  	 
		
		cap drop front_var

		if "`samp'"=="sampEW_1890" | "`samp'"=="sampMIDWEST_EW1890" | "`samp'"=="sampSOUTH_EW1890" | "`samp'"=="sampWEST_EW1890"  {
		   g front_var =  tye_tfe890_500kNI_100_l6
		}
		if "`samp'"=="sampEW_pd1950g2" | "`samp'"=="sampMIDWEST_pd1950g2" | "`samp'"=="sampSOUTH_pd1950g2" | "`samp'"=="sampWEST_pd1950g2"  {
		   g front_var = tye_tfe950_500kNI_100_l6
		}		
		lab var front_var "total frontier experience"
				
		preserve 
		
			qui xi: reg avgrep2000to2016 $otherX i.statea if `samp'==1
			keep if e(sample)

			eststo c_`samp' : xi: reg propertytaxrate2010 front_var $otherX i.statea, $SE  
			qui distinct fips if e(sample)
			estadd scalar numC = r(ndistinct)
			estadd ysumm
			
			eststo d_`samp' : xi: reg avgrep2000to2016 front_var $otherX i.statea, $SE  
			qui distinct fips if e(sample)
			estadd scalar numC = r(ndistinct)
			estadd ysumm

		restore

	}	
	
	#d ;
	esttab c_* using "$outdirapp/tableB7-propertytaxrate2010.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( front_var , relax)
		keep( front_var , relax) varwidth(30)
		$STAR 						
		mlabels(, none) collabels(, none) coeflabels(front_var "total frontier experience")
		stats(numC ymean, fmt(%9.0fc %9.2f)
		labels("Number of Counties" "Mean of Dependent Variable" ))
		substitute(_ \_) style(tex) prefoot( & & & & & & & & \\ \hline ) postfoot( \hline );	

	esttab d_* using "$outdirapp/tableB7-avgrep2000to2016.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( front_var , relax)
		keep( front_var , relax) varwidth(30)
		$STAR 						
		mlabels(, none) collabels(, none) coeflabels(front_var "total frontier experience")
		stats(numC ymean, fmt(%9.0fc %9.2f)
		labels("Number of Counties" "Mean of Dependent Variable" ))
		substitute(_ \_) style(tex) prefoot( & & & & & & & & \\ \hline ) postfoot(\hline );		
	#d cr		
