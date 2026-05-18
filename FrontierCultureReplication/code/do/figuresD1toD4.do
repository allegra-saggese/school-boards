
*FIGURE D1
use "$dir/data/immigrationAnnual", clear
	twoway (line lmig2 year if year>=1790 & year<=1890, color(gs0) msymbol(i) alpattern(dash) lpattern(solid) alwidth(thick) lwidth(thick) legend(size(small))), ///
       ytitle("annual inflow of immigrants to the U.S.") ysca(titlegap(2)) xlabel(1790(10)1890) legend(off) xtitle("") plotregion(color(white)) ylabel(,angle(360))

	graph export "$outdirapp/figureD1.pdf", replace	

*FIGURE D2a
use "$dir/data/centerPopShift", clear

	twoway (scatter dcpshift decavelmig2 if decade_tag==1 &  decavelmig2<=600000, mlabel(decade) msize(small) msymbol(circle) mcolor(black) mlabcolor(black) xscale(range(0 600000)))  ///
		  (lfit dcpshift decavelmig2 if decade_tag==1 &  decavelmig2<=600000 , color(gs0) msymbol(i) alpattern(dash) lpattern(solid) alwidth(thick) lwidth(thick)  xscale(range(0 600000))), ///
			ytitle("westward shift in center of population (in km)") legend(off) title("")  xscale(range(0 600000)) plotregion(color(white)) ylabel(,angle(360))
	
	graph export "$outdirapp/figureD2a.pdf", replace
	
*FIGURE D2b
use "$dir/data/selectionInstrument", clear

	twoway lfit totKids lmig2 , lwidth(medthick) lcolor(gs0) || scatter totKids lmig2, msymbol(i) mlabel(moveyr) mlabcolor(gs0) mlabsize(small) plotregion(color(white)) ///
			xtitle("immigrant arrivals to the United States") ytitle("number of children moving to the frontier") legend(off) ylabel(,angle(360)) 
			
	graph export "$outdirapp/figureD2b.pdf", replace
	
*FIGURE D3
use "$dir/data/immigrationAnnual", clear

	twoway (line eumig year if year>=1820 & year<=1890, color(gs0) msymbol(i) alpattern(dash) lpattern(solid) alwidth(thick) lwidth(thick)) ///
		(line pmig1 year if year>=1820 & year<=1890, color(gs6) msymbol(i) alpattern(dash) lpattern(dash) alwidth(thick) lwidth(thick)), ///
       ytitle("annual inflow of immigrants to the U.S.") ysca(titlegap(2)) xlabel(1820(10)1890) xtitle("") plotregion(color(white)) ylabel(,angle(360)) ///
	   legend(label(1 "actual") label(2 "predicted") row(2) ring(0) pos(12) region(color(white)))

	graph export "$outdirapp/figureD3.pdf", replace

*FIGURE D4
use "$dir/data/selectionInstrument", clear
	
	keep if pt10n>0
	
	twoway lfit pt10n totKids, lwidth(medthick) lcolor(gs0) || scatter pt10n totKids, msymbol(i) mlabel(moveyr) mlabcolor(gs0) mlabsize(small) plotregion(color(white)) ///
			xtitle("number of native-born children moving to the frontier") ytitle("mean infrequent names" "among native-born children moving to frontier") legend(off) ylabel(,angle(360))
	graph export "$outdirapp/figureD4a.pdf",replace
			
	twoway lfit pt10n lmig2, lwidth(medthick) lcolor(gs0) || scatter pt10n lmig2, msymbol(i) mlabel(moveyr) mlabcolor(gs0) mlabsize(small) plotregion(color(white)) ///
			xtitle("immigrants to the United States") ytitle("mean infrequent names" "among native-born children moving to frontier") legend(off) ylabel(,angle(360)) 
	graph export "$outdirapp/figureD4b.pdf",replace
	
	