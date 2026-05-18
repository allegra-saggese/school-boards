
	eststo clear
	
	eststo : reg Xuname10_1880 frontierMigrant mXuname10_1880 fXuname10_1880 i.male##i.age i.birthOrder if frontier1870_L6==0 & frontierNonMigrant==0 & nativity==11, cl(cty1870)
	su Xuname10_1880 if frontierMigrant==0 & e(sample)
	estadd scalar mu=r(mean)
	
	eststo : reg Xuname10_1880 frontier1880_L6 mXuname10_1880 fXuname10_1880 i.male##i.age i.birthOrder if nativity==11, cl(cty1870) 
	su Xuname10_1880 if frontier1880_L6==0 & e(sample)
	estadd scalar mu=r(mean)
	
	eststo : reg Xuname10_1880 frontierNonMigrant frontierMigrant mXuname10_1880 fXuname10_1880 i.male##i.age i.birthOrder if nativity==11, cl(cty1870) 
	su Xuname10_1880 if (nonfrontierNonMigrant==1 | nonfrontierMigrant==1) & e(sample)
	estadd scalar mu=r(mean)
	
	test frontierNonMigrant=frontierMigrant
	estadd scalar pval = r(p)
	eststo : reg Xuname10_1880 frontierNonMigrant mXuname10_1880 fXuname10_1880 i.male##i.age i.birthOrder if frontier1880_L6==1 & nativity==11, cl(cty1870) 
	su Xuname10_1880 if frontierMigrant==1 & e(sample)
	estadd scalar mu=r(mean)

	#d ;
	esttab using "$outdirnber/tableI1.tex", replace noobs nomtitle nodepvar nonumber plain fragment label
		cells(b(fmt(3) $STARCELL) se(par fmt(3)))
		order(frontierMigrant frontier1880_L6 frontierNonMigrant, relax)
		keep(frontier1880_L6 frontierNonMigrant frontierMigrant, relax) varwidth(30)
		$STAR 				
		mlabels(, none) collabels(, none) prehead( & & & &\\ )
		substitute(_ \_) style(tex) prefoot( & & & & \\ \hline)		
		stats(N mu r2, fmt(%9.0fc %9.3f %9.2f ) labels("Number of Individuals" "Mean Infrequent Name Share" "R\$^2\$"));
	#d cr
