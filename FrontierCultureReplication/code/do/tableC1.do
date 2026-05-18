	
	foreach dep in BIGGOV TAX BUDGET {
		
		eststo clear
		
		forvalues j=190(3)199 {

			eststo : reghdfe `dep'Share TFE $otherX if y==`j', absorb(year D2MA D3ENC D4WNC D5SA D6ESC D7WSC D8MT D9PC) cluster(CD)
			estadd ysumm

		}
				
		#d ;
		esttab using "$outdirapp/tableC1-`dep'.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
			cells(b(fmt(3) $STARCELL) se(par fmt(3)))
			order( TFE , relax)
			keep( TFE , relax) varwidth(30)
			$STAR						
			mlabels(, none) collabels(, none) prehead( & & & & \\ )
			substitute(_ \_) style(tex) prefoot( & & & & \\ \hline)	
			stats(N r2, fmt( %9.0fc %9.2f )
    		labels("Number of Counties" "R\$^2\$"  )) ;
		#d cr	
			
	}
	
