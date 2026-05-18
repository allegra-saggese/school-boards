
est drop _all

global frontierX tye_tfe890_500kNI_100_l6
lab var $frontierX "total frontier experience"

qui xi: reg shkpt10d $frontierX $otherX i.statea if sampE_W1790E1890==1
keep if e(sample)

*standardize outcome
foreach v of varlist shkpt10n shkpt10d shkpt10s shkpt10c shkpt100d shmkpt10n shmkpt10d shmkpt10s shmkpt10c shmkpt100d patmat patmat1 {
	su `v'
	replace `v' = (`v' - r(mean))/r(sd)
}
	
*make pair FE for density
	global pdX pd_1940		
	cap drop pdRank* pair*
	sort statea $pdX
	by statea: g pdRanks = _n
	g pairFEs = .
	levelsof statea, loc(sloc)
	foreach s of local sloc {			
		su pdRanks if statea==`s'
		forvalues j=2(2)`=r(max)' {
			replace pairFEs = `j' if (pdRanks==`j' | pdRanks==`=`j'-1') & statea==`s'
		}
		su pairFEs if statea==`s'
		replace pairFEs = r(max) if statea==`s' & pairFEs==. & $pdX<.
	}
	egen pairFEstateDens = group(pairFEs statea)		
	
*make pair FE for immigrant share
	global pdX shfrb1940
	cap drop pdRank* pairFEs 
	sort statea $pdX
	by statea: g pdRanks = _n
	g pairFEs = .
	levelsof statea, loc(sloc)
	foreach s of local sloc {			
		su pdRanks if statea==`s'
		forvalues j=2(2)`=r(max)' {
			replace pairFEs = `j' if (pdRanks==`j' | pdRanks==`=`j'-1') & statea==`s'
		}
		su pairFEs if statea==`s'
		replace pairFEs = r(max) if statea==`s' & pairFEs==. & $pdX<.
	}
	egen pairFEstateImmig = group(pairFEs statea)	
	
eststo clear
	
foreach dep in shkpt10n shkpt10d shkpt10s shkpt10c shkpt100d shmkpt10n shmkpt10d shmkpt10s shmkpt10c shmkpt100d patmat patmat1 {

	xi: reg `dep' $frontierX i.statea, $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]	
	
*panel (a)
	eststo a`dep' : xi: reg `dep' $frontierX $otherX i.statea, $SE
	estadd ysumm			
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
*panel (b)
	eststo b`dep' : xi: reghdfe `dep' $frontierX $otherX, cluster(km_grid_cel_code pairFEstateDens) absorb(pairFEstateDens)  
	estadd ysumm			
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
*panel (c)
	eststo c`dep' : xi: reghdfe `dep' $frontierX $otherX, cluster(km_grid_cel_code pairFEstateImmig) absorb(pairFEstateImmig)  
	estadd ysumm			
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2)))
	
}	

*panel (a)
#d ;
esttab a* using "$outdirapp/tableB3a.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
	order( $frontierX , relax)
	keep( $frontierX , relax) varwidth(30)
	$STAR						
	mlabels(, none) collabels(, none) prehead( & & & & & & & & & & & & \\ )
	substitute(_ \_) style(tex) prefoot( & & & & & & & & & &  & & \\ \hline)	
	stats(ostDelta N r2, fmt(%9.2f %9.0fc %9.2f )
	labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "R\$^2\$"));
#d cr	
	
*panel (b)
#d ;
esttab b* using "$outdirapp/tableB3b.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
	order( $frontierX , relax)
	keep( $frontierX , relax) varwidth(30)
	$STAR						
	mlabels(, none) collabels(, none) prehead( & & & & & & & & & & & & \\ )
	substitute(_ \_) style(tex) prefoot( & & & & & & & & & &  & & \\ \hline)	
	stats(ostDelta N r2, fmt(%9.2f %9.0fc %9.2f )
	labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "R\$^2\$"));
#d cr	
	
*panel (c)
#d ;
esttab c* using "$outdirapp/tableB3c.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
	order( $frontierX , relax)
	keep( $frontierX , relax) varwidth(30)
	$STAR						
	mlabels(, none) collabels(, none) prehead( & & & & & & & & & & & & \\ )
	substitute(_ \_) style(tex) prefoot( & & & & & & & & & &  & & \\ \hline)	
	stats(ostDelta N r2, fmt(%9.2f %9.0fc %9.2f )
	labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "R\$^2\$"));
#d cr	
	