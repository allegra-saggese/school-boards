
eststo clear

foreach dep in shkpt10d shmkpt10d { 

	eststo : xi: reg `dep' frontier100kmL6 i.year D1NE D2MA D3ENC D4WNC D5SA D6ESC D7WSC D8MT D9PC if year<=1890 & orig==1, cl(km_grid_cel_code) 
	su `dep' if e(sample) & frontier100kmL6==0
	estadd scalar mu = r(mean)    

	eststo : xi: teffects nnmatch (`dep' D1NE D2MA D3ENC D4WNC D5SA D6ESC D7WSC D8MT D9PC shrfrb) (frontier100kmL6) if year<=1890 & orig==1, ///
		atet nn(1) vce(robust) metric(euclidean) ematch(year) 
	su `dep' if e(sample) & frontier100kmL6==0
	estadd scalar mu = r(mean)
	
}	
#d ;
esttab using "$outdirapp/tableB1.tex", replace noobs nomtitle nodepvar nonumber plain fragment 
	cells(b(fmt(3) $STARLEVEL) se(par fmt(3)))
	order(frontier100kmL6 r1vs0.frontier100kmL6, relax) keep(frontier100kmL6 r1vs0.frontier100kmL6, relax) varlabels(r1vs0.frontier100kmL6 "frontier county")
	$STAR						
	mlabels(, none) collabels(, none) prehead( & & & &  \\ )
	substitute(_ \_) style(tex) prefoot( & & & & \\ \hline)	
	stats(N mu, fmt(%9.0fc %9.3f )
	labels("Number of County-Years" "Mean Dep. Var. in Non-Frontier Counties"  ));
#d cr 		
