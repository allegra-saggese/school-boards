
eststo clear

global IFF if (year<=1890 & orig==1) | (inrange(year,1900,1930) & sampE_W1790E1890==1)

foreach depvar in wsexrat w15_49_shr fb_shr w20plusillit_shr shkpt10d shmkpt10d {

	preserve
	
	keep if `depvar'<.	    		   
					   
	xi: regress `depvar' ib3.dofe500kNI_l6100_cat20 i.year D1NE D2MA D3ENC D4WNC D5SA2 D6ESC D7WSC D8MT D9PC $IFF, cluster(km_grid_cel_code)

	estimates store `depvar'

	local label: variable label `depvar'

	qui coefplot `depvar', keep(*dofe500kNI_l6100_cat20*) omitted baselevels ytitle("`label'" "relative to decade of frontier exit", ///
		) mcolor(black) yline(0,lpattern(dot) lcolor(gs0)) ciopts(recast(rcap) color(black) lpattern(solid) lcolor(black) lwidth(medthin)) ///
		vertical levels(95) xtitle(Decade Before/After Leaving the Frontier) plotregion(color(white)) ylabel(,angle(360))

	qui graph export "$outdirapp/figureG1-`depvar'.pdf", replace			   
	  
	restore

}

