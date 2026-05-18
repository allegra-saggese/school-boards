
*panel (a)

eststo clear	

global FE D1NE D2MA D3ENC D4WNC D5SA D6ESC D7WSC D8MT D9PC

foreach depvar in  wsexrat w15_49_shr fb_shr w20plusillit_shr shkpt10d shmkpt10d {     

	eststo : xi: reg `depvar' frontier100kmL6 i.year $FE if year<=1890 & orig==1, cl(km_grid_cel_code) 
	su `depvar' if e(sample) & frontier100kmL6==0
	estadd scalar mu = r(mean)
	
}	
#d ;
esttab using "$outdir/table1a.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
	keep(frontier100kmL6) 
	$STAR						
	mlabels(, none) collabels(, none) prehead( & & & & & & \\ )
	substitute(_ \_) style(tex) prefoot( & & & & & &  \\ \hline)	
	stats(mu N r2, fmt(%9.2f %9.0fc %9.2f )
	labels("Mean Dep. Var. in Non-Frontier Counties" "Number of County-Years" "R\$^2\$"));
#d cr 

*panel (b)

eststo clear	

foreach depvar in wsexrat w15_49_shr fb_shr w20plusillit_shr shkpt10d shmkpt10d {  
   
	eststo : xi: reg `depvar' frontier100km popDensLess6 i.year $FE if year<=1890 & orig==1, cl(km_grid_cel_code) 
	su `depvar' if e(sample) & frontier100kmL6==0
	estadd scalar mu = r(mean)
	
}	

#d ;
esttab using "$outdir/table1b.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(3) $STARCELL) se(par fmt(3)))
	keep(frontier100km popDensLess6) 
	$STAR						
	mlabels(, none) collabels(, none) prehead( & & & & & & \\ )
	substitute(_ \_) style(tex) prefoot( & & & & & &  \\ \hline)	
	stats(mu N r2, fmt(%9.2f %9.0fc %9.2f )
	labels("Mean Dep. Var. in Non-Frontier Counties" "Number of County-Years" "R\$^2\$"));
#d cr 
