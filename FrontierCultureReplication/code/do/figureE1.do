
cap program drop gphMake
program define gphMake
	#d ;
	qui coefplot, keep($KEEPX) vertical yline(0,lpattern(dot) lcolor(gs0)) ylabel(,angle(360)) msymbol(O) color(gs0) 
					ciopts(recast(rcap) lwidth(medthick) lpattern(solid) color(gs0 gs0)) 
					plotregion(color(white)) ytitle("{&beta}{sub:j} with 95% confidence interval" " " "Dep. Var.: child has an infrequent name") levels(95) xtitle("year of birth relative to arrival in frontier county") 
					xline(8.5,lpattern(dash) lwidth(thin) lcolor(gs8)) basel 
					coeflabels( 
			13.yearsInFrontierRoundPos="-8" 
			14.yearsInFrontierRoundPos="-7"
			15.yearsInFrontierRoundPos="-6"
			16.yearsInFrontierRoundPos="-5"
			17.yearsInFrontierRoundPos="-4"
			18.yearsInFrontierRoundPos="-3"
			19.yearsInFrontierRoundPos="-2"
			20.yearsInFrontierRoundPos="-1"
			21.yearsInFrontierRoundPos="1"
			22.yearsInFrontierRoundPos="2"
			23.yearsInFrontierRoundPos="3"
			24.yearsInFrontierRoundPos="4"
			25.yearsInFrontierRoundPos="5"
			26.yearsInFrontierRoundPos="6"
			27.yearsInFrontierRoundPos="7"
			28.yearsInFrontierRoundPos="8"
			29.yearsInFrontierRoundPos="9"
			30.yearsInFrontierRoundPos="10"
			31.yearsInFrontierRoundPos="11"
			32.yearsInFrontierRoundPos="12"
			33.yearsInFrontierRoundPos="13"
			34.yearsInFrontierRoundPos="14"
			35.yearsInFrontierRoundPos="15");		
		
	graph export "$outdirnber/figureE1-${panel}.pdf", replace;	
	#d cr
end

	global KEEPX 
	forvalues v=13(1)35 {
		global KEEPX $KEEPX `v'.yearsInFrontierRoundPos
	}
	
	global DEP top10FnamNtvpAl20AllDv
				
	*(a)
	global panel a
	reghdfe $DEP b20.yearsInFrontierRoundPos if cf==1, absorb(sex famid birthDec) cluster(nhgisjoin) 
	gphMake
	
	*(b)
	global panel b
	reghdfe $DEP b20.yearsInFrontierRoundPos if cf==1, absorb(sex famid birthDec2) cluster(nhgisjoin) 
	gphMake
	
	*(c)
	global panel c
	reghdfe $DEP b20.yearsInFrontierRoundPos if cf==1, absorb(sex famid birthDec3) cluster(nhgisjoin) 
	gphMake
	
	*(d)
	global panel d
	reghdfe $DEP b20.yearsInFrontierRoundPos relchorder1b3_a6 if cf==1, absorb(sex famid) cluster(nhgisjoin) 
	gphMake
	
	*(e)
	global panel e
	reghdfe $DEP b20.yearsInFrontierRoundPos relchorder1b3_a6 if cf==1, absorb(sex famid birthDec3) cluster(nhgisjoin) 
	gphMake

	*(f)
	global panel f
	reghdfe $DEP b20.yearsInFrontierRoundPos mutop10FnamNtvpAl20AllDv if cf==1, absorb(sex famid) cluster(nhgisjoin) 
	gphMake
