
global frontierX tye_tfe890_500kNI_100_l6
lab var $frontierX "total frontier experience"

qui xi: reg shkpt10d $frontierX $otherX i.statea if sampE_W1790E1890==1
keep if e(sample)

*standardize outcome
foreach v of varlist shkpt10d shmkpt10d {
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
	
	
foreach dep of varlist shkpt10d shmkpt10d { 		
		
	eststo clear	
		
	*(1)
	xi: reg `dep' $frontierX i.statea, $SE
	eststo `dep'1
	estadd ysumm			
	local maxR = min(e(r2)*1.3,1)
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
	
	*(2)
	global FEstate 
	foreach s in 5 8 12 13 17 18 19 20 21 22 26 27 28 29 31 35 36 37 38 39 40 42 46 47 48 51 54 55 56 {
		cap drop FEs`s'
		g FEs`s' = statea==`s'
		global FEstate $FEstate FEs`s'
	}
	eststo `dep'2 : xi: reg `dep' $frontierX $otherX $FEstate, $SE
	estadd ysumm			
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): `dep'2

	*(3)
	eststo `dep'3 : xi: reghdfe `dep' $frontierX $otherX, cluster(km_grid_cel_code pairFEstateDens) absorb(pairFEstateDens)  
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): `dep'3

	*(4)		
	eststo `dep'4 : xi: reghdfe `dep' $frontierX $otherX, cluster(km_grid_cel_code pairFEstateImmig) absorb(pairFEstateImmig)  
	estadd ysumm
	estadd scalar ostDelta = (_b[$frontierX]/(${baseTFE}-_b[$frontierX]))*((e(r2)-${baseR2})/(0.3*e(r2))): `dep'4

	#d ;
	esttab `dep'* using "$outdir/table2-`dep'.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( $frontierX , relax)
		keep( $frontierX , relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none) prehead( & & & & \\ )
		substitute(_ \_) style(tex) prefoot( & & & &  \\ \hline)	
		stats(ostDelta N r2, fmt(%9.2f %9.0fc %9.2f )
		labels("Oster $\delta$ for $\beta=0$" "Number of Counties" "R\$^2\$"));
	#d cr
}


