
*panel (a)
	eststo clear

	foreach d in pt10n pt10d pt10s pt10c pt100d mtpt10n mtpt10d mtpt10s mtpt10c mtpt100d patmat patmat1 {
	
		eststo: reghdfe `d' TFE $otherX, cl(km_grid_cel_code) absorb(age statefip chorder sex)
		estadd ysumm
		
	}
		
	#d ;
	esttab * using "$outdirnber/tableB4a.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order(TFE, relax)
		keep(TFE, relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none) prehead( & & & & & & & & & & & & \\ )
		substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & \\ \hline)		
		stats(N ymean r2, fmt(%12.0fc %9.3f %9.2f )
		labels("Number of Individuals" "Dep. Var. Mean" "R\$^2\$"));
	#d cr	

*panel (b)
	eststo clear

	foreach d in pt10n pt10d pt10s pt10c pt100d mtpt10n mtpt10d mtpt10s mtpt10c mtpt100d patmat patmat1 {
	
		eststo: reghdfe `d' TFE $otherX, cl(km_grid_cel_code) absorb(age statefip chorder sex pairPDens)
		estadd ysumm
		
	}
		
	#d ;
	esttab * using "$outdirnber/tableB4b.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order(TFE, relax)
		keep(TFE, relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none) prehead( & & & & & & & & & & & & \\ )
		substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & \\ \hline)		
		stats(N ymean r2, fmt(%12.0fc %9.3f %9.2f )
		labels("Number of Individuals" "Dep. Var. Mean" "R\$^2\$"));
	#d cr	

*panel (c)
	eststo clear

	foreach d in pt10n pt10d pt10s pt10c pt100d mtpt10n mtpt10d mtpt10s mtpt10c mtpt100d patmat patmat1 {
	
		eststo: reghdfe `d' TFE $otherX, cl(km_grid_cel_code) absorb(age statefip chorder sex pairImmig)
		estadd ysumm
		
	}
		
	#d ;
	esttab * using "$outdirnber/tableB4c.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order(TFE, relax)
		keep(TFE, relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none) prehead( & & & & & & & & & & & & \\ )
		substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & \\ \hline)		
		stats(N ymean r2, fmt(%12.0fc %9.3f %9.2f )
		labels("Number of Individuals" "Dep. Var. Mean" "R\$^2\$"));
	#d cr	
}

*panel (d)
	eststo clear

	foreach d in pt10n pt10d pt10s pt10c pt100d mtpt10n mtpt10d mtpt10s mtpt10c mtpt100d patmat patmat1 {
	
		eststo: reghdfe `d' TFE $otherX, cl(km_grid_cel_code) absorb(age statefip chorder sex Lnam)
		estadd ysumm
		
	}
		
	#d ;
	esttab * using "$outdirnber/tableB4d.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order(TFE, relax)
		keep(TFE, relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none) prehead( & & & & & & & & & & & & \\ )
		substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & \\ \hline)		
		stats(N ymean r2, fmt(%12.0fc %9.3f %9.2f )
		labels("Number of Individuals" "Dep. Var. Mean" "R\$^2\$"));
	#d cr	

*panel (e)
	eststo clear

	foreach d in pt10n pt10d pt10s pt10c pt100d mtpt10n mtpt10d mtpt10s mtpt10c mtpt100d patmat patmat1 {
	
		eststo: reghdfe `d' TFE $otherX, cl(km_grid_cel_code) absorb(age statefip chorder sex pairPDens Lnam)
		estadd ysumm
		
	}
		
	#d ;
	esttab * using "$outdirnber/tableB4e.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order(TFE, relax)
		keep(TFE, relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none) prehead( & & & & & & & & & & & & \\ )
		substitute(_ \_) style(tex) prefoot( & & & & & & & & & & & & \\ \hline)		
		stats(N ymean r2, fmt(%12.0fc %9.3f %9.2f )
		labels("Number of Individuals" "Dep. Var. Mean" "R\$^2\$"));
	#d cr	
