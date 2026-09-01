# Descriptive series for the Data section: the trends the paper is about,
# before any model or regression touches them.
suppressMessages({library(data.table); library(ggplot2)})
source("functions.R"); source("R/paths.R")

d <- fread(data_path("processed","panel","model_input_households.csv"),
           select=c("YEAR","HHWT","f_h","m_h","f_lab","m_lab","f_emp","m_emp"),
           showProgress=FALSE)
d[, `:=`(works = as.numeric(f_h > 0),
         zW = fifelse((f_lab+m_lab)>0, f_lab/(f_lab+m_lab), NA_real_),
         outearn = as.numeric(f_lab > m_lab))]
s <- d[, .(
  lfp      = 100*weighted.mean(works, HHWT),
  share    = weighted.mean(zW, HHWT, na.rm=TRUE),
  outearn  = 100*weighted.mean(outearn, HHWT),
  hrs_gap  = weighted.mean(m_h - f_h, HHWT)
), by=YEAR][order(YEAR)]
s[, era := ifelse(YEAR %in% c(1980,1990,2000), "Decennial census", "ACS")]

pd <- rbindlist(list(
  s[, .(YEAR, era, v=lfp,          panel="(a) Wives with positive annual hours (%)")],
  s[, .(YEAR, era, v=100*share,    panel="(b) Wife's share of couple labor earnings (%)")],
  s[, .(YEAR, era, v=outearn,      panel="(c) Couples where the wife out-earns (%)")],
  s[, .(YEAR, era, v=hrs_gap,      panel="(d) Husband minus wife annual hours")]))
pd[, panel := factor(panel, levels=unique(panel))]

save_plot("bkp_pure_descriptive_trends.png", {
  print(ggplot(pd, aes(YEAR, v)) +
    geom_line(colour="#08519C", linewidth=0.9) +
    geom_point(aes(shape=era), colour="#08519C", size=2.1, fill="white", stroke=0.9) +
    scale_shape_manual(values=c("Decennial census"=21, "ACS"=19)) +
    facet_wrap(~panel, scales="free_y", ncol=2) +
    labs(x=NULL, y=NULL, shape="Sample") +
    theme_minimal(base_size=13) +
    theme(legend.position="top", panel.grid.minor=element_blank(),
          strip.text=element_text(face="bold", size=11),
          panel.spacing=unit(1.2,"lines")))
}, width=2300, height=1400)

cat("Key descriptive series, married couples both 18-65\n\n")
cat(sprintf("%-6s %10s %12s %12s %10s\n","year","wife works","wife share","out-earns","hours gap"))
for (y in c(1980,1990,2000,2010,2020,2024)) {
  r <- s[YEAR==y]
  cat(sprintf("%-6d %9.1f%% %11.3f %11.1f%% %10.0f\n", y, r$lfp, r$share, r$outearn, r$hrs_gap))
}
b <- s[YEAR==1980]; e <- s[YEAR==2024]
cat(sprintf("\n1980->2024:  wife works %+.1f pp | share %+.3f | out-earns %+.1f pp | gap %+.0f hrs\n",
    e$lfp-b$lfp, e$share-b$share, e$outearn-b$outearn, e$hrs_gap-b$hrs_gap))
fwrite(s, dated_path(data_path("processed","results"), "bkp_pure_descriptive_trends.csv"))
