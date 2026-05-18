
foreach depvar in wsexrat w15_49_shr fb_shr w20plusillit_shr shkpt10d shmkpt10d {

	preserve
	
	keep if `depvar'<. & df500kNI_bothkm >-100 & sampE_W1790E1890==1 & year<=1890
	
	xi: semipar `depvar' i.year D1NE D2MA D3ENC D4WNC D5SA2 D6ESC D7WSC D8MT D9PC, kernel(epan) nonpar(df500kNI_bothkm) partial(`depvar'_d_yrdvfe) degree(1) ci nograph	
	
	local label: variable label `depvar'
	
	twoway lpolyci `depvar'_d_yrdvfe df500kNI_bothkm if df500kNI_bothkm <400, deg(1) kernel(epan) level(95) xtitle("Distance to Frontier", margin(medsmall)) ///
					ytitle("E(`label' | year & division FE)", size(medsmall)) subtitle("", size(small)) legend(off)  color(gs0) ciplot(rconnected) msymbol(i) alpattern(dash) ///
					lpattern(solid) alwidth(thick) lwidth(thick) plotregion(color(white)) ylabel(,angle(360)) xlabel(-100(100)400)
					
	graph export "$outdirapp/figureG2-`depvar'.pdf", replace
	
	restore
}	


