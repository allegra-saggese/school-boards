
eststo clear

keep if oneFamObs==1 & top10ffnam!=.

global IFNOTfarm if farmer==0

	eststo : reghdfe focscorus pt10nMU frontierNoDensXpt10nMU, cl(nhgisjoin) absorb(i.year#i.nhgisjoin numKids)
	estadd ysumm

	eststo : reghdfe focscorus top10ffnam frontierNoDensXtop10ffnam, cl(nhgisjoin) absorb(i.year#i.nhgisjoin numKids)
	estadd ysumm

	eststo : reghdfe focscorus top10ffnam frontierNoDensXtop10ffnam pt10nMU frontierNoDensXpt10nMU, cl(nhgisjoin) absorb(i.year#i.nhgisjoin numKids)
	estadd ysumm

	eststo : reghdfe focscorus top10ffnam frontierNoDensXtop10ffnam pt10nMU frontierNoDensXpt10nMU $IFNOTfarm, cl(nhgisjoin) absorb(i.year#i.nhgisjoin numKids)
	estadd ysumm

#d ;
	esttab using "$outdirnber/table9.tex", 
		replace noobs nomtitle nodepvar plain fragment label  
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order(pt10nMU frontierNoDensXpt10nMU top10ffnam frontierNoDensXtop10ffnam, relax)
		keep(pt10nMU frontierNoDensXpt10nMU top10ffnam frontierNoDensXtop10ffnam, relax) varwidth(30)
		$STAR					
		mlabels(, none) collabels(, none) 
		stats(N ymean r2, fmt(%9.0fc %9.1f %9.2f)
		labels("Observations" "Mean Dep. Var." "R\$^2\$"))
		substitute(_ \_) style(tex) prefoot( & & & & \\ \hline );
#d cr
