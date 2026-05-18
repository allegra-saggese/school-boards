
foreach depvar in taxLocalPop pubdebLocalPop POSTOFFICE rail existcanal { 

	preserve
	
	keep if `depvar'<. & year<=1890 & sampE_W1790E1890==1 & popdens<100 
	
	xi: semipar `depvar' D1NE D2MA D3ENC D4WNC D5SA2 D6ESC D7WSC D8MT D9PC i.year, kernel(epan) nonpar(popdens) partial(`depvar'h) degree(1) ci nograph	
	
	local label: variable label `depvar'
	
	twoway lpolyci `depvar'h popdens if popdens<50, deg(1) kernel(epan) level(95) xtitle("Population Density", margin(medsmall)) ytitle("E(`label' | year & division FE)") ///
					subtitle("", size(small)) plotregion(color(white)) ylabel(,angle(360)) legend(off) color(gs0) ///
					ciplot(rconnected) msymbol(i) alpattern(dash) lpattern(solid) alwidth(thick) lwidth(thick)
					
	graph export "$outdirapp/figureG3-`depvar'.pdf", replace

	restore
	
}	


