# Within-couple time series: hours, earnings, wages, wife vs husband.
# The point of the figure is the flat male line -- the norm's threshold IS his
# earnings, so his stagnation is why more couples now hit the constraint.
suppressMessages({library(data.table); library(ggplot2)})
source("functions.R"); source("R/paths.R")

d <- fread(data_path("processed","panel","model_input_households.csv"),
           select=c("YEAR","HHWT","f_h","m_h","f_lab","m_lab","f_w","m_w",
                    "f_w_predicted","m_w_predicted"), showProgress=FALSE)
wq <- function(x,w,p=.5){i<-order(x);x<-x[i];w<-w[i];x[which.max(cumsum(w)/sum(w)>=p)]}

hrs <- d[, .(Wife = weighted.mean(f_h,HHWT), Husband = weighted.mean(m_h,HHWT)), by=YEAR]
ern <- d[, .(Wife = weighted.mean(deflate_to(f_lab,YEAR,2024),HHWT),
             Husband = weighted.mean(deflate_to(m_lab,YEAR,2024),HHWT)), by=YEAR]
wg  <- d[m_w_predicted==FALSE & f_w_predicted==FALSE,
         .(Wife = wq(deflate_to(f_w,YEAR,2024),HHWT),
           Husband = wq(deflate_to(m_w,YEAR,2024),HHWT)), by=YEAR]

mk <- function(x,lab) melt(x, id.vars="YEAR", variable.name="spouse",
                           value.name="v")[, panel := lab][]
pd <- rbindlist(list(mk(hrs,"Annual market hours"),
                     mk(ern,"Annual labour earnings (2024 $)"),
                     mk(wg, "Median hourly wage (2024 $)")))
pd[, panel := factor(panel, levels=c("Annual market hours",
                                     "Annual labour earnings (2024 $)",
                                     "Median hourly wage (2024 $)"))]
pd[, era := ifelse(YEAR %in% c(1980,1990,2000), "Decennial census", "ACS")]

save_plot("t3_hours_earnings_wife_vs_husband.png", {
  print(ggplot(pd, aes(YEAR, v, colour=spouse)) +
    geom_line(linewidth=1.0) +
    geom_point(aes(shape=era), size=2.2, fill="white", stroke=0.9) +
    scale_shape_manual(values=c("Decennial census"=21,"ACS"=19)) +
    scale_colour_manual(values=c(Wife="#B2182B", Husband="#08519C")) +
    facet_wrap(~panel, scales="free_y", ncol=3) +
    expand_limits(y=0) +
    labs(title="Within married couples: the wife converges, the husband does not move",
         subtitle=paste0("Married couples, both spouses 18-65, IPUMS 1980-2024. Wages are medians among couples with BOTH\n",
                         "wages observed; hours and earnings are means over all couples. Real series deflated to 2024 dollars.\n",
                         "Men's real median wage rose 8% in 44 years -- and was flat (index 100-101) from 1980 to 2015."),
         x=NULL, y=NULL, colour=NULL, shape="Sample",
         caption=paste0("Why this matters for the model: the breadwinner norm's threshold IS his earnings. His stagnation, ",
                        "not only her gains,\nis what pushed the share of couples the norm binds on from 15.9% to 30.3%. ",
                        "Had his wages kept pace it would be 18.5%.")) +
    theme_minimal(base_size=13) +
    theme(plot.title=element_text(face="bold", size=15),
          plot.subtitle=element_text(colour="grey30", size=10.5),
          plot.caption=element_text(colour="grey45", size=9, hjust=0),
          legend.position="top", panel.grid.minor=element_blank(),
          strip.text=element_text(face="bold"), panel.spacing=unit(1.4,"lines")))
}, width=2600, height=1150)

cat("\n1980 vs 2024, indexed:\n")
for (nm in c("hrs","ern","wg")) {
  x <- get(nm); b <- x[YEAR==1980]; e <- x[YEAR==2024]
  cat(sprintf("  %-34s wife %+5.0f%%   husband %+5.0f%%\n",
      c(hrs="annual hours", ern="annual earnings (real)", wg="hourly wage (real)")[nm],
      100*(e$Wife/b$Wife-1), 100*(e$Husband/b$Husband-1)))
}
