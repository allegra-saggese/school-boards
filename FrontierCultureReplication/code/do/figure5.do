			
global frontierX tye_tfe890_500kNI_100_l6			
			
g TFEdem_u = .
g TFEdem_b = .
g TFEdem_l = .
g TFErep_u = .
g TFErep_b = .
g TFErep_l = .
g TFEnondr_u = .
g TFEnondr_b = .
g TFEnondr_l = .
g sig95 = .
g year = 1896 + _n*4 
replace year = . if year>2016 
		
forvalues year=1900(4)2016 {

	xi: reghdfe pvotrep`year' $frontierX $otherX , $SE absorb(statea)
	
	replace TFErep_u = _b[$frontierX] + 2*_se[$frontierX] if year==`year'
	replace TFErep_b = _b[$frontierX] if year==`year'
	replace TFErep_l = _b[$frontierX] - 2*_se[$frontierX] if year==`year'
	
	noi test ${frontierX}=0
	replace sig95 = r(p)<0.05  if year==`year'

}			

#d;

twoway (scatter TFErep_b year if sig95==0, mcolor(gs0) msymbol(O) msize(medsmall))
		(scatter TFErep_b year if sig95==1, mcolor(red) msymbol(O) msize(medsmall))
	(line TFErep_u TFErep_b TFErep_l year, sort lpattern(dash solid dash) lcolor(gs0 gs0 gs0))
	if inrange(year,1900,2016), legend(off) plotregion(color(white)) ylabel(,angle(360)) 
	yline(0,lpattern(shortdash) lcolor(gs8)) xlabel(1900(10)2010) 
	ytitle("TFE effect +/- 2 x std. error") xtitle("")
	;
graph export "$outdir/figure5.pdf", replace;

#d cr
