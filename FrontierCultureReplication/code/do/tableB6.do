
*panels (a) and (b)
use "$dir/data/names1940", clear

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
	
	qui xi: reg shkpt10d $frontierX $otherX i.statea if sampE_W1790E1890==1
	keep if e(sample)
	
	foreach dep of varlist shkpt10d shmkpt10d {
	
		su `dep'
		replace `dep' = (`dep' - r(mean))/r(sd)
		
		eststo clear
		
		xi: reg `dep' $frontierX i.statea, $SE
		global baseR2 = e(r2)
		global baseTFE = _b[$frontierX]
		
		global i=1
		eststo x1 : xi: reg `dep' $frontierX $otherX i.statea, $SE  
		tabAdd			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=2
		eststo x2 : xi: reg `dep' $frontierX $otherX tri_ave i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=3
		eststo x3 : xi: reg `dep' $frontierX $otherX ppt_risk i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=4
		eststo x4 : xi: reg `dep' $frontierX $otherX d_port i.statea, $SE  
		tabAdd		
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=5
		eststo x5 : xi: reg `dep' $frontierX $otherX d_mrdspre1890 i.statea, $SE  
		tabAdd		
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))

		global i=6
		eststo x6 : xi: reg `dep' $frontierX $otherX d_batt i.statea, $SE  
		tabAdd			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=7
		eststo x7 : xi: reg `dep' $frontierX $otherX shslav1860 i.statea, $SE  
		tabAdd			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=8
		eststo x8 : xi: reg `dep' $frontierX $otherX wsexrat1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=9
		eststo x9 : xi: reg `dep' $frontierX $otherX fb_shr1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=10
		eststo x10 : xi: reg `dep' $frontierX $otherX fbscotirel_shr1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=11
		eststo x11 : xi: reg `dep' $frontierX $otherX bplfrac_1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=12
		eststo x12 : xi: reg `dep' $frontierX $otherX yearswithRRbef1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=13
		eststo x13 : xi: reg `dep' $frontierX $otherX shempmanu1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
						
		global i=14
		eststo x14 : xi: reg `dep' $frontierX $otherX $otherXnewPre i.statea, $SE  
		tabAdd			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))

		#d ;
		esttab using "$outdirapp/tableB6-`dep'.tex", 
			replace noobs nomtitle nodepvar plain fragment label 
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX , relax)
			keep( $frontierX , relax) varwidth(30)
			$STAR						
			mlabels(, none) collabels(, none)
			stats(ostDelta N r2, fmt(%9.2f %9.0fc %9.2f )
			labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "R\$^2\$"))	
			substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & & & \\ \hline );		
		#d cr							
		
		#d ;
		esttab using "$outdirapp/tableJ3-`dep'.tex", 
			replace noobs nomtitle nodepvar plain fragment label 
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX $otherXnewPre, relax)
			keep( $frontierX $otherXnewPre, relax) varwidth(30)
			$STAR						
			mlabels(, none) collabels(, none)
			stats(ostDelta N r2, fmt(%9.2f %9.0fc %9.2f )
			labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "R\$^2\$"))	
			substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & & & \\ \hline );		
		#d cr	
		
	}
		
*panel (c)
use "$dir/data/cces.dta", clear 

	local dep muCCES

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
		
	noi xi: reg `dep' $frontierX $indX $otherX $FEs if sampE_W1790E1890==1, $SE 
	keep if e(sample)		
	
	eststo clear
	
	xi: reg `dep' $frontierX i.statea, $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	
	global i=1
	eststo x1 : xi: reg `dep' $frontierX $indX $otherX i.statea i.year, $SE  
	tabAdd			
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=2
	eststo x2 : xi: reg `dep' $frontierX $indX $otherX tri_ave i.statea i.year, $SE  
	tabAdd	
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=3
	eststo x3 : xi: reg `dep' $frontierX $indX $otherX ppt_risk i.statea i.year, $SE  
	tabAdd	
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=4
	eststo x4 : xi: reg `dep' $frontierX $indX $otherX d_port i.statea i.year, $SE  
	tabAdd		
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=5
	eststo x5 : xi: reg `dep' $frontierX $indX $otherX d_mrdspre1890 i.statea i.year, $SE  
	tabAdd		
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))

	global i=6
	eststo x6 : xi: reg `dep' $frontierX $indX $otherX d_batt i.statea i.year, $SE  
	tabAdd			
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=7
	eststo x7 : xi: reg `dep' $frontierX $indX $otherX shslav1860 i.statea i.year, $SE  
	tabAdd			
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=8
	eststo x8 : xi: reg `dep' $frontierX $indX $otherX wsexrat1890 i.statea i.year, $SE  
	tabAdd	
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=9
	eststo x9 : xi: reg `dep' $frontierX $indX $otherX fb_shr1890 i.statea i.year, $SE  
	tabAdd	
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=10
	eststo x10 : xi: reg `dep' $frontierX $indX $otherX fbscotirel_shr1890 i.statea i.year, $SE  
	tabAdd	
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=11
	eststo x11 : xi: reg `dep' $frontierX $indX $otherX bplfrac_1890 i.statea i.year, $SE  
	tabAdd	
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=12
	eststo x12 : xi: reg `dep' $frontierX $indX $otherX yearswithRRbef1890 i.statea i.year, $SE  
	tabAdd	
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
	global i=13
	eststo x13 : xi: reg `dep' $frontierX $indX $otherX shempmanu1890 i.statea i.year, $SE  
	tabAdd	
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
					
	global i=14
	eststo x14 : xi: reg `dep' $frontierX $indX $otherX $otherXnewPre i.statea i.year, $SE  
	tabAdd			
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))

	#d ;
	esttab using "$outdirapp/tableB6-`dep'.tex", 
		replace noobs nomtitle nodepvar plain fragment label 
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( $frontierX , relax)
		keep( $frontierX , relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none)
		stats(ostDelta N ymean r2, fmt(%9.2f %9.0fc %9.2f %9.2f )
		labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "Mean of Dependent Variable" "R\$^2\$"))	
		substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & & & \\ \hline );		
	#d cr							
	
	#d ;
	esttab using "$outdirapp/tableJ3-`dep'.tex", 
		replace noobs nomtitle nodepvar plain fragment label 
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( $frontierX $otherXnewPre, relax)
		keep( $frontierX $otherXnewPre, relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none)
		stats(ostDelta N ymean r2, fmt(%9.2f %9.0fc %9.2f %9.2f )
		labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "Mean of Dependent Variable" "R\$^2\$"))	
		substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & & & \\ \hline );		
	#d cr		
		
*panel (d) and (e)
use "$dir/data/proptaxvote",clear

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"

	qui xi: reg avgrep2000to2016 $frontierX $otherX i.statea if sampE_W1790E1890==1
	keep if e(sample)
	
	foreach dep of varlist propertytaxrate2010 avgrep2000to2016  {
		
		eststo clear
		
		xi: reg `dep' $frontierX i.statea, $SE
		global baseR2 = e(r2)
		global baseTFE = _b[$frontierX]
		
		global i=1
		eststo x1 : xi: reg `dep' $frontierX $otherX i.statea, $SE  
		tabAdd			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
		global i=2
		eststo x2 : xi: reg `dep' $frontierX $otherX tri_ave i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
		global i=3
		eststo x3 : xi: reg `dep' $frontierX $otherX ppt_risk i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=4
		eststo x4 : xi: reg `dep' $frontierX $otherX d_port i.statea, $SE  
		tabAdd		
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=5
		eststo x5 : xi: reg `dep' $frontierX $otherX d_mrdspre1890 i.statea, $SE  
		tabAdd		
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))

		global i=6
		eststo x6 : xi: reg `dep' $frontierX $otherX d_batt i.statea, $SE  
		tabAdd			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=7
		eststo x7 : xi: reg `dep' $frontierX $otherX shslav1860 i.statea, $SE  
		tabAdd			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=8
		eststo x8 : xi: reg `dep' $frontierX $otherX wsexrat1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=9
		eststo x9 : xi: reg `dep' $frontierX $otherX fb_shr1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=10
		eststo x10 : xi: reg `dep' $frontierX $otherX fbscotirel_shr1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=11
		eststo x11 : xi: reg `dep' $frontierX $otherX bplfrac_1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=12
		eststo x12 : xi: reg `dep' $frontierX $otherX yearswithRRbef1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
		
		global i=13
		eststo x13 : xi: reg `dep' $frontierX $otherX shempmanu1890 i.statea, $SE  
		tabAdd	
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
						
		global i=14
		eststo x14 : xi: reg `dep' $frontierX $otherX $otherXnewPre i.statea, $SE  
		tabAdd			
		estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))

		#d ;
		esttab using "$outdirapp/tableB6-`dep'.tex", 
			replace noobs nomtitle nodepvar plain fragment label 
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX , relax)
			keep( $frontierX , relax) varwidth(30)
			$STAR						
			mlabels(, none) collabels(, none)
			stats(ostDelta N ymean r2, fmt(%9.2f %9.0fc %9.2f %9.2f )
			labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "Mean of Dependent Variable" "R\$^2\$"))	
			substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & & & \\ \hline );		
		#d cr							
		
		#d ;
		esttab using "$outdirapp/tableJ3-`dep'.tex", 
			replace noobs nomtitle nodepvar plain fragment label 
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX $otherXnewPre, relax)
			keep( $frontierX $otherXnewPre, relax) varwidth(30)
			$STAR						
			mlabels(, none) collabels(, none)
			stats(ostDelta N ymean r2, fmt(%9.2f %9.0fc %9.2f %9.2f )
			labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "Mean of Dependent Variable" "R\$^2\$"))	
			substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & & & \\ \hline );		
		#d cr	
		
	}	
