

*TABLE D.1
	eststo clear	
	
	foreach iv in lavipmig1500kNoIs_110d30 lavmig2500kNoIs_110d30 {
		  
		eststo clear

		*names
		use "$dir/data/names1940", clear

		global frontierX tye_tfe890_500kNI_100_l6
		lab var $frontierX "total frontier experience"
		
		qui xi: reg shkpt10d $frontierX $otherX i.statea if sampE_W1790E1890==1
		keep if e(sample)
		
		foreach dep in shkpt10d patmat {
			su `dep'
			replace `dep' = (`dep' - r(mean))/r(sd)
		}
	
		eststo : xi: weakiv ivreg2 shkpt10d $otherX i.statea ( $frontierX = `iv'), $SE small estadd null(0) partial(i.statea $otherX) ///
			first savefirst savefprefix(a)
		estadd ysumm		

		eststo : xi: weakiv ivreg2 patmat $otherX i.statea ( $frontierX = `iv'), $SE small estadd null(0) partial(i.statea $otherX) ///
			first savefirst savefprefix(b)
		estadd ysumm				
				 
		*mu cces			
		use "$dir/data/cces",clear

		keep if sampE_W1790E1890==1

		global frontierX tye_tfe890_500kNI_100_l6
		lab var $frontierX "total frontier experience"
	
		eststo : xi: weakiv ivreg2 muCCES $otherX $indX i.statea i.year ( $frontierX = `iv'), $SE small estadd null(0) partial(i.statea $otherX $indX) ///
			first savefirst savefprefix(c)
		estadd ysumm					
				 
		*property tax and republican vote
		use "$dir/data/proptaxvote",clear

		keep if sampE_W1790E1890==1

		global frontierX tye_tfe890_500kNI_100_l6
		lab var $frontierX "total frontier experience"

		eststo : xi: weakiv ivreg2 propertytaxrate2010 $otherX i.statea ( $frontierX = `iv'),  $SE small estadd null(0) partial(i.statea $otherX) ///
			first ffirst savefirst savefprefix(d)
		estadd ysumm
			
		eststo : xi: weakiv ivreg2 avgrep2000to2016 $otherX i.statea ( $frontierX = `iv'),  $SE small estadd null(0) partial(i.statea $otherX) ///
			first ffirst savefirst savefprefix(e)
		estadd ysumm

		#d ;
		esttab using "$outdirapp/tableD1-`iv'.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX , relax)
			keep( $frontierX , relax) varwidth(30)
			$STAR
			mlabels(, none) collabels(, none) 
			stats(N ymean widstat, fmt(%9.0fc %9.2f %9.2f)
			labels("Number of Observations" "Mean of Dependent Variable" "First Stage F Statistic"))
			substitute(_ \_) style(tex) prefoot( & & & & & \\ \hline );		
		#d cr	

	}	

*TABLE D.2
use "$dir/data/proptaxvote",clear

	keep if sampE_W1790E1890==1

	lab var tye_tfe890_500kNI_100_l6 "total frontier experience"			
	label var lavmig2500kNoIs_110d30 "Log Average Actual National Migration Inflows"
	label var lavipmig1500kNoIs_110d30 "Log Average Predicted National Migration Inflows"
	label var lat "Latitude"
	label var lon "Longitude"
	label var ave_gyi "Average Agricultural Suitability"
	
	replace d_coa=d_coa/1000
	replace d_riv=d_riv/1000
	replace d_lak=d_lak/1000

	eststo : xi: ivreg2 avgrep2000to2016 $otherX i.statea ( $frontierX = lavmig2500kNoIs_110d30),  $SE small partial(i.statea) ///
		first ffirst savefirst savefprefix(ma)			
	matrix first = e(first)
	estadd scalar F_stat = first[4, 1]: ma*		

	eststo : xi: ivreg2 avgrep2000to2016 $otherX i.statea ( $frontierX = lavipmig1500kNoIs_110d30), $SE small partial(i.statea) ///
		first ffirst savefirst savefprefix(ia)			
	matrix first = e(first)
	estadd scalar F_stat = first[4, 1]: ia*		

	#d ;
	esttab ma* ia* using "$outdirapp/tableD2.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( lavmig2500kNoIs_110d30 lavipmig1500kNoIs_110d30 $otherX , relax)
		keep(lavmig2500kNoIs_110d30 lavipmig1500kNoIs_110d30 $otherX, relax) varwidth(30)
		$STAR
		mlabels(, none) collabels(, none) 
		stats(N F_stat, fmt(%9.0f %9.2f)
		labels("Number of Counties" "First Stage F Statistic"))
		substitute(_ \_) style(tex) prefoot( & & \\ \hline );		
	#d cr		

