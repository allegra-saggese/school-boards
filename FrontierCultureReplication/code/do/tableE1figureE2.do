
eststo clear

	cap program drop tabAdd
	program define tabAdd
		distinct famid50 if e(sample)
		estadd scalar numF = r(ndistinct)	
		estadd ysumm	
	end	
	
local regvar ageatmoveyr150nonneg	
lab var ageatmoveyr150nonneg "age-at-move to frontier"
	
keep if All==1 & `regvar'<=17 
	
*----------------------*
*FIGURE F2
*----------------------*
#d ;

*panel (a);

	eststo : reghdfe top10FnamNtvpAl20AllDv ib1.`regvar' chorder if allmatches50_onlyMover==1 [pw=IPW], absorb(dupmatches50 sex famid50 relchorder150) cluster(famid50 Ct);
		
	coefplot , keep(*`regvar'*) vertical yline(0,lpattern(dot) lcolor(gs0)) ylabel(,angle(360)) ciopts(recast(rcap) 
				lwidth(medthick) color(gs0)) color(gs0) xscale(titlegap(*5)) yscale(titlegap(*-20)) 
				plotregion(color(white)) ytitle("{&beta}{sub:j} with 95% confidence interval" " " "Dep. Var.: child has infrequent name, 1880 Census") 
				levels(95) xtitle("age that father moved to frontier, 1850 Census") 
				basel coeflabels( 0.`regvar'= "0" 1.`regvar'= "1" 2.`regvar' = "2" 3.`regvar'="3" 4.`regvar'="4" 
				5.`regvar' ="5" 6.`regvar'="6" 7.`regvar'="7" 8.`regvar'="8" 
				9.`regvar' ="9" 10.`regvar'="10" 11.`regvar'="11" 12.`regvar'="12" 13.`regvar' ="13" 14.`regvar' ="14" 
				15.`regvar' ="15" 16.`regvar' ="16" 17.`regvar' ="17" 18.`regvar' ="18" 19.`regvar' ="19" 20.`regvar' ="20");
	
	qui graph export "$outdirnber/figureE2a.pdf", replace;
	
*panel (b);

	eststo : reghdfe top10FnamNtvpAl20AllDv ib0.`regvar' chorder if allmatches50_allKids==1, absorb(dupmatches50 sex famid50 relchorder150) cluster(famid50 Ct);
		
	coefplot , keep(*`regvar'*) vertical yline(0,lpattern(dot) lcolor(gs0)) ylabel(,angle(360)) ciopts(recast(rcap) 
				lwidth(medthick) color(gs0)) color(gs0) xscale(titlegap(*5)) yscale(titlegap(*-20)) 
				plotregion(color(white)) ytitle("{&beta}{sub:j} with 95% confidence interval" " " "Dep. Var.: child has infrequent name, 1880 Census") 
				levels(95) xtitle("age that father moved to frontier, 1850 Census") 
				basel coeflabels( 0.`regvar'= "0" 1.`regvar'= "1" 2.`regvar' = "2" 3.`regvar'="3" 4.`regvar'="4" 
				5.`regvar' ="5" 6.`regvar'="6" 7.`regvar'="7" 8.`regvar'="8" 
				9.`regvar' ="9" 10.`regvar'="10" 11.`regvar'="11" 12.`regvar'="12" 13.`regvar' ="13" 14.`regvar' ="14" 
				15.`regvar' ="15" 16.`regvar' ="16" 17.`regvar' ="17" 18.`regvar' ="18" 19.`regvar' ="19" 20.`regvar' ="20");
	
	qui graph export "$outdirnber/figureE2b.pdf", replace;

#d cr
	
*----------------------*
*TABLE F1
*----------------------*
	
*panel (a)

eststo clear

preserve
keep if allmatches50_onlyMover==1 

	*(1)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder [pw=IPW], absorb(dupmatches50 sex famid50 relchorder150) cluster(famid50 Ct)
	qui tabAdd
	
	*(2)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder [pw=IPW], absorb(dupmatches50 sex famid50 relchorder150 St) cluster(famid50 Ct)
	qui tabAdd
	
	*(3)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder [pw=IPW], absorb(dupmatches50 sex famid50 relchorder150 birthDec) cluster(famid50 Ct)
	qui tabAdd
	
	*(4)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder [pw=IPW], absorb(dupmatches50 sex famid50 relchorder150 birthDec2) cluster(famid50 Ct)
	qui tabAdd
	
	*(5)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder [pw=IPW], absorb(dupmatches50 sex famid50 relchorder150 birthDec3) cluster(famid50 Ct)
	qui tabAdd

 #d ;
	esttab using "$outdirnber/tableE1a.tex", 
		replace noobs nomtitle nodepvar plain fragment label  
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( `regvar', relax)
		keep( `regvar', relax) varwidth(30) 
		$STAR					
		mlabels(, none) collabels(, none) 
		stats(N numF ymean r2, fmt(%9.0fc  %9.0fc  %9.2f %9.2f) 
		labels("Observations" "Number of Families" "Mean of Dependent Variable" "R\$^2\$")) 
		substitute(_ \_) style(tex) prefoot( & & & & & \\ \hline );
#d cr

restore	
	
*panel (b)

eststo clear

preserve
keep if allmatches50_allKids==1 

	*(1)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder, absorb(dupmatches50 sex famid50 relchorder150) cluster(famid50 Ct)
	qui tabAdd
	
	*(2)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder, absorb(dupmatches50 sex famid50 relchorder150 St) cluster(famid50 Ct)
	qui tabAdd
	
	*(3)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder, absorb(dupmatches50 sex famid50 relchorder150 birthDec) cluster(famid50 Ct)
	qui tabAdd
	
	*(4)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder, absorb(dupmatches50 sex famid50 relchorder150 birthDec2) cluster(famid50 Ct)
	qui tabAdd
	
	*(5)
	eststo : reghdfe top10FnamNtvpAl20AllDv `regvar' chorder, absorb(dupmatches50 sex famid50 relchorder150 birthDec3) cluster(famid50 Ct)
	qui tabAdd

 #d ;
	esttab using "$outdirnber/tableE1b.tex", 
		replace noobs nomtitle nodepvar plain fragment label  
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( `regvar', relax)
		keep( `regvar', relax) varwidth(30) 
		$STAR					
		mlabels(, none) collabels(, none) 
		stats(N numF ymean r2, fmt(%9.0fc  %9.0fc  %9.2f %9.2f) 
		labels("Observations" "Number of Families" "Mean of Dependent Variable" "R\$^2\$")) 
		substitute(_ \_) style(tex) prefoot( & & & & & \\ \hline );
#d cr	

restore
