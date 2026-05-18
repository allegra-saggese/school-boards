
global Xcounty log_area_2010 lat lon temp_mean rain_mean elev_mean d_coa d_riv d_lak ave_gyi

global IFF if inner==1 | (frontier100km==1 & inner==0)

eststo clear

foreach depvar in infectiousDiseaseGR anySickness { 
 
	eststo : xi: reg `depvar' frontier100kmL6 $Xcounty D1NE D2MA D3ENC D4WNC D5SA D6ESC D7WSC D8MT D9PC $IFF, cl(km_grid_cel_code) 
	estadd ysumm
	
	eststo : xi: reg `depvar' frontier100km popDensLess6 $Xcounty D1NE D2MA D3ENC D4WNC D5SA D6ESC D7WSC D8MT D9PC $IFF, cl(km_grid_cel_code) 
	estadd ysumm
	
}

#d ;
esttab using "$outdirapp/tableJ4.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
	cells(b(fmt(4) $STARCELL) se(par fmt(4)))
	order(frontier100kmL6 frontier100km popDensLess6, relax) keep(frontier100kmL6 frontier100km popDensLess6, relax) 
	$STAR						
	mlabels(, none) collabels(, none) prehead( & & & & \\ )
	substitute(_ \_) style(tex) prefoot( & & & & \\ \hline)	
	stats(ymean N r2, fmt(%9.3f %9.0fc %9.2f )
	labels("Mean Dependent Variable" "Number of Counties" "R\$^2\$"));
#d cr 
