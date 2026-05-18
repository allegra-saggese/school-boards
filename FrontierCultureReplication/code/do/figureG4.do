
local depvar gini

*panel (a)

	preserve
	
	keep if `depvar'<. & year<=1890 & sampE_W1790E1890==1 & popdens<100 
	
	xi: semipar `depvar' D1NE D2MA D3ENC D4WNC D5SA2 D6ESC D7WSC D8MT D9PC i.year, kernel(epan) nonpar(popdens) partial(`depvar'h) degree(1) ci nograph	
	
	local label: variable label `depvar'
	
	twoway lpolyci `depvar'h popdens if popdens<50, deg(1) kernel(epan) level(95) xtitle("Population Density", margin(medsmall)) ytitle("E(`label' | year & division FE)") ///
					subtitle("", size(small)) plotregion(color(white)) ylabel(,angle(360)) legend(off) color(gs0) ///
					ciplot(rconnected) msymbol(i) alpattern(dash) lpattern(solid) alwidth(thick) lwidth(thick)
					
	graph export "$outdirapp/figureG4a.pdf", replace

	restore
	

*panel (b)

eststo clear

global IFF if (year<=1890 & orig==1) | (inrange(year,1900,1930) & sampE_W1790E1890==1)

	preserve
	
	keep if `depvar'<. & year<1940 	    		   
					   
	xi: regress `depvar' ib3.dofe500kNI_l6100_cat20 i.year D1NE D2MA D3ENC D4WNC D5SA2 D6ESC D7WSC D8MT D9PC $IFF, cluster(km_grid_cel_code)

	estimates store `depvar'

	local label: variable label `depvar'

	qui coefplot `depvar', keep(*dofe500kNI_l6100_cat20*) omitted baselevels ytitle("`label' relative to decade of frontier exit", ///
		size(small)) yline(0,lpattern(dot) lcolor(gs0)) ciopts(recast(rcap) color(black) lpattern(solid) lcolor(black) lwidth(medthin)) ///
		vertical levels(95) xtitle(Decade Before/After Leaving the Frontier) plotregion(color(white)) ylabel(,angle(360))

	qui graph export "$outdirapp/figureG4b.pdf", replace			   
	  
	restore

