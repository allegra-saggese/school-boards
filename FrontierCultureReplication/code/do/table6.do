
eststo clear

global IFin if ((bornAfterMoveToFront1==0 & cf==1 & frontierMigrant1==1) | (cf==0 & bornAfterMoveToFront1==. & frontierNonMigrant1==0))
global IFout if ((bornAfterMoveToSettl1==0 & cf==0 & nonfrontierMigrant1==1) | (cf==1))
	
	eststo : reghdfe pt10n frontierMigrant1 $IFin, cl(nhgisjoin) absorb(i.sex#i.age#i.year chorder)
	su pt10n if e(sample) & frontierMigrant1==0
	estadd scalar muDep = r(mean)
	
	eststo : reghdfe pt10n frontierMigrant1 $IFin, cl(nhgisjoin) absorb(i.sex#i.age#i.year bplst chorder)
	su pt10n if e(sample) & frontierMigrant1==0
	estadd scalar muDep = r(mean)

	eststo : reghdfe pt10n nonfrontierMigrant1 $IFout, cl(nhgisjoin) absorb(i.sex#i.age#i.year chorder)
	su pt10n if e(sample) & nonfrontierMigrant1==0
	estadd scalar muDep = r(mean)
	
	eststo : reghdfe pt10n nonfrontierMigrant1 $IFout, cl(nhgisjoin) absorb(i.sex#i.age#i.year bplst chorder)
	su pt10n if e(sample) & nonfrontierMigrant1==0
	estadd scalar muDep = r(mean)
	
*nb: sample size is same in cols 2 and 4 as 1 and 3 but reghdfe does not report singleton obs (within FE) in the overall sample size	
	
#d ;
	esttab using "$outdirnber/table6.tex", 
		replace noobs nomtitle nodepvar plain fragment label 
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order( frontierMigrant1 nonfrontierMigrant1,relax) 
		keep( frontierMigrant1 nonfrontierMigrant1, relax) varwidth(30)
		$STAR						
		mlabels(, none) collabels(, none) 
		stats(N muDep r2, fmt(%9.0fc %9.2f %9.2f)
		labels("Observations" "Mean of Dep. Var., Stayers" "R\$^2\$")) 
		substitute(_ \_) style(tex) prefoot( & & & & \\ \hline );
#d cr
