
eststo clear

*panels (a) and (b)
use "$dir/data/names1940", clear

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
	
	lab var totYear_pdLess6_1790to1890 "total low density experience"

	qui xi: reg shkpt10d $frontierX $otherX i.statea if sampE_W1790E1890==1
	keep if e(sample)
		
	*make deciles of density
	g pd1940d_state = .
	levelsof statea, loc(sloc)
	foreach s of local sloc {
		cap xtile ps = pd_1940 if statea==`s', n(10)
		cap replace pd1940d_state = ps if statea==`s'
		cap drop ps		
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
	
	foreach dep of varlist shkpt10d shmkpt10d {
	
		g _`dep' = `dep'
	
		su `dep'
		replace `dep' = (`dep' - r(mean))/r(sd)

		xi: reg `dep' $frontierX i.statea, $SE
		global baseR2 = e(r2)
		global baseTFE = _b[$frontierX]

		eststo clear
		
		global i=1
		eststo x1 : xi: reg `dep' $frontierX $otherX i.statea, $SE  
		tabAdd
		
		global i=2
		eststo x2 : xi: reg `dep' $frontierX $otherX pd_1940 i.statea, $SE  
		tabAdd		
		
		global i=3
		eststo x3 : xi: reg `dep' $frontierX $otherX i.pd1940d_state i.statea, $SE  
		tabAdd	
		
		global i=4
		eststo x4 : xi: reghdfe `dep' $frontierX $otherX, cluster(km_grid_cel_code pairFEstate) absorb(pairFEstateDens)  
		tabAdd
		
		global i=5
		local p=55
		qui reg `dep' $frontierX $otherX i.statea if shurbpop1940>=`p', $SE 
		qui su _`dep' if e(sample)
		qui replace `dep' = (_`dep' - r(mean))/r(sd) if e(sample)
		su shurbpop1940,d
		eststo x5 : xi: reg `dep' $frontierX $otherX i.statea if shurbpop1940>=`p', $SE  
		tabAdd	

		global i=6
		qui reg `dep' $frontierX $otherX i.statea if shurbpop1940<`p', $SE 
		qui su _`dep' if e(sample)
		qui replace `dep' = (_`dep' - r(mean))/r(sd) if e(sample)
		eststo x6 : xi: reg `dep' $frontierX $otherX i.statea if shurbpop1940<`p', $SE  
		tabAdd		

		global i=7
		qui reg `dep' $frontierX $otherX i.statea totYear_pdLess6_1790to1890, $SE 
		qui su _`dep' if e(sample)
		qui replace `dep' = (_`dep' - r(mean))/r(sd) if e(sample)
		eststo x7 : xi: reg `dep' $otherX $frontierX totYear_pdLess6_1790to1890 i.statea, $SE  
		tabAdd	
		
		#d ;
		esttab using "$outdir/table4-`dep'.tex", 
			replace noobs nomtitle nodepvar nonumber plain fragment label
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX totYear_pdLess6_1790to1890 , relax)
			keep( $frontierX  totYear_pdLess6_1790to1890 , relax) varwidth(30)
			$STAR
			mlabels(, none) collabels(, none)
			stats(N r2, fmt(%9.0fc %9.2f )
			labels("Number of Counties" "R\$^2\$"))	
			substitute(_ \_) style(tex) prefoot( & & & & & & & \\ \hline );		
		#d cr
		
	}


*panel (c)

cap program drop pdM
program define pdM
	cap drop pdRank* pairFE*
		preserve
		keep if e(sample)
		noi su year 
		bys fips: g keepOne = _n==1
		keep if keepOne==1
		keep statea pd_2000 fips
		sort statea pd_2000
		by statea: g pdRanks = _n
		g pairFEs = .
		levelsof statea, loc(sloc)
		foreach s of local sloc {			
			su pdRanks if statea==`s'
			forvalues j=2(2)`=r(max)' {
				replace pairFEs = `j' if (pdRanks==`j' | pdRanks==`=`j'-1') & statea==`s'
			}
			su pairFEs if statea==`s'
			replace pairFEs = r(max) if statea==`s' & pairFEs==.
		}
		egen pairFEstate = group(pairFEs statea)
		keep pairFEstate fips
		tempfile m
		save `m'
		restore
	merge m:1 fips using `m'
	drop _m
end	

use "$dir/data/cces.dta", clear 

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
	
	lab var totYear_pdLess6_1790to1890 "total low density experience"
	
	noi xi: reg muCCES $frontierX $indX $otherX $FEs if sampE_W1790E1890==1, $SE 
	keep if e(sample)

	bys gisjoin: g uni=_n==1
	su shurbpop2000 if uni==1,d

	g pd2000d_state=.
	levelsof statea, loc(sloc)
	foreach s of local sloc {
		cap xtile ps = pd_2000 if statea==`s' & uni==1, n(10)
		cap replace pd2000d_state = ps if statea==`s' & uni==1
		cap drop ps
	}	
	bys gisjoin: egen _pd_2000d_state = min(pd2000d_state)
	replace pd2000d_state = _pd_2000d_state
	
	pdM

	local p = 79.08152
	global ifPhi if shurbpop2000>=`p'
	global ifPlo if shurbpop2000<`p'
	global pdx pd_2000
	global pdxd pd2000d_state					

	eststo clear
	
	xi: reg muCCES $frontierX i.statea, $SE
	global baseR2 = e(r2)
	global baseTFE = _b[$frontierX]
		
	global i=1
	eststo x1 : xi: reg muCCES $frontierX $indX $otherX i.year i.statea, $SE  
	tabAdd
	
	global i=2
	eststo x2: xi: reg muCCES $frontierX $indX $otherX $pdx i.year i.statea, $SE  
	tabAdd		
	
	global i=3
	eststo x3 : xi: reg muCCES $frontierX $indX $otherX i.$pdxd i.year i.statea, $SE  
	tabAdd		
	
	global i=4
	noi eststo x4 : xi: reghdfe muCCES $frontierX $indX $otherX i.year, cluster(km_grid_cel_code pairFEstate) absorb(pairFEstate)  
	tabAdd
	
	global i=5
	eststo x5 : xi: reg muCCES $frontierX $indX $otherX i.statea i.year $ifPhi, $SE  
	tabAdd	
	
	global i=6
	eststo x6 : xi: reg muCCES $frontierX $indX $otherX i.statea i.year $ifPlo, $SE  
	tabAdd		
	
	global i=7
	eststo x7 : xi: reg muCCES $otherX $indX $frontierX totYear_pdLess6_1790to1890 i.statea i.year, $SE  
	tabAdd					
	
	#d ;
	esttab using "$outdir/table4-muCCES.tex", 
			replace noobs nomtitle nodepvar nonumber plain fragment label
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX totYear_pdLess6_1790to1890 , relax)
			keep( $frontierX  totYear_pdLess6_1790to1890 , relax) varwidth(30)
			$STAR
			mlabels(, none) collabels(, none)
			stats(N ymean r2, fmt(%9.0fc %9.2f %9.2f )
		labels("Number of Individuals" "Mean of Dependent Variable" "R\$^2\$"))	
		substitute(_ \_) style(tex)  prefoot( & & & & & & & \\ \hline );		
	#d cr

*panel (d) and (e)
foreach dep in propertytaxrate2010 avgrep2000to2016  {
	
	use "$dir/data/proptaxvote",clear

	global frontierX tye_tfe890_500kNI_100_l6
	lab var $frontierX "total frontier experience"
	
	lab var totYear_pdLess6_1790to1890 "total low density experience"

	*qui xi: reg `dep' $frontierX $otherX i.statea if sampE_W1790E1890==1
	*keep if e(sample)	
	keep if sampE_W1790E1890==1
	
	*make deciles of density
	g pd2000d_state = .
	g pd2010d_state = .
	levelsof statea, loc(sloc)
	foreach s of local sloc {
		cap xtile ps = pd_2000 if statea==`s', n(10)
		cap replace pd2000d_state = ps if statea==`s'
		cap drop ps		
		cap xtile ps = pd_2010 if statea==`s', n(10)
		cap replace pd2010d_state = ps if statea==`s'
		cap drop ps		
	}	
		
	*make pair FE for density
		global pdX pd_2000		
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
		egen pairFEstateDens2000 = group(pairFEs statea)	
		
		global pdX pd_2010		
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
		egen pairFEstateDens2010 = group(pairFEs statea)		

		if "`dep'"=="avgrep2000to2016" {
			local p = 78.41705
			local p = 76.8
			global ifPhi if shurbpop2000>=`p'
			global ifPlo if shurbpop2000<`p'
			global pdx pd_2000
			global pdxd pd2000d_state					
			global pdFE pairFEstateDens2000
		}
		else {
			local p = 81.82176
			local p = 80
			global ifPhi if shurbpop2010>=`p'
			global ifPlo if shurbpop2010<`p'
			global pdx pd_2010
			global pdxd pd2010d_state
			global pdFE pairFEstateDens2010
		}
		
		eststo clear
		
		xi: reg `dep' $frontierX i.statea, $SE
		global baseR2 = e(r2)
		global baseTFE = _b[$frontierX]
		
		global i=1
		eststo x1 : xi: reg `dep' $frontierX $otherX i.statea, $SE  
		tabAdd

		global i=2
		eststo x2 : xi: reg `dep' $frontierX $otherX $pdx i.statea, $SE  
		tabAdd		
		
		global i=3
		eststo x3 : xi: reg `dep' $frontierX $otherX i.$pdxd i.statea, $SE  
		tabAdd		
		
		global i=4
		eststo x4 : xi: reghdfe `dep' $frontierX $otherX, cluster(km_grid_cel_code $pdFE) absorb($pdFE)  
		tabAdd
		
		global i=5
		eststo x5 : xi: reg `dep' $frontierX $otherX i.statea $ifPhi, $SE  
		tabAdd	
		
		global i=6
		eststo x6 : xi: reg `dep' $frontierX $otherX i.statea $ifPlo, $SE  
		tabAdd		
		
		global i=7
		eststo x7 : xi: reg `dep' $otherX $frontierX totYear_pdLess6_1790to1890 i.statea, $SE  
		tabAdd					
		
		#d ;
		esttab using "$outdir/table4-`dep'.tex", 
			replace noobs nomtitle nodepvar nonumber plain fragment label
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( $frontierX totYear_pdLess6_1790to1890 , relax)
			keep( $frontierX  totYear_pdLess6_1790to1890 , relax) varwidth(30)
			$STAR
			mlabels(, none) collabels(, none)
			stats(N ymean r2, fmt(%9.0fc %9.2f %9.2f )
			labels("Number of Counties" "Mean of Dependent Variable" "R\$^2\$"))	
			substitute(_ \_) style(tex) prefoot( & & & & & & & \\ \hline );				
		#d cr	
		
	}
