# Standalone regeneration of the tau figure. Plain-text title (no plotmath),
# explicit white background, and a PDF alongside the PNG for LaTeX inclusion.
suppressMessages({library(data.table); library(ggplot2)})
source("functions.R"); source("R/paths.R")
rd <- data_path("processed","results")
t  <- fread(sort(list.files(rd,"t3_tau_series.csv$",full.names=TRUE))[1])
t[, era := factor(ifelse(YEAR %in% c(1980,1990,2000), "Decennial census","ACS"),
                  levels=c("Decennial census","ACS"))]
pd <- melt(t[, .(YEAR, era,
                 `All households` = tau_model,
                 `Households the norm binds on` = tau_binding)],
           id.vars=c("YEAR","era"), variable.name="measure", value.name="tau")

p <- ggplot(pd, aes(YEAR, tau, colour=measure)) +
  geom_line(linewidth=0.95) +
  geom_point(aes(shape=era), size=2.5, fill="white", stroke=1) +
  scale_shape_manual(values=c("Decennial census"=21, "ACS"=19)) +
  scale_colour_manual(values=c("All households"="#08519C",
                               "Households the norm binds on"="#B2182B")) +
  scale_y_continuous(limits=c(0,NA), labels=function(x) paste0(round(100*x), "%")) +
  scale_x_continuous(breaks=seq(1980,2020,10)) +
  labs(title = "The norm wedge over time, 1980-2024",
       subtitle = paste0("tau = alpha x C: the implicit tax the breadwinner norm places on the wife's marginal earnings.\n",
                         "Reported as tau rather than alpha because alpha falls 89% over this period, most of which\n",
                         "is nominal income growth rather than any weakening of the norm."),
       x=NULL, y="Implicit tax on her marginal earnings",
       colour=NULL, shape="Sample",
       caption="Source: IPUMS USA 1980-2024, 16.9 million couples. One structural estimate per year.") +
  theme_minimal(base_size=13) +
  theme(plot.background  = element_rect(fill="white", colour=NA),
        panel.background = element_rect(fill="white", colour=NA),
        plot.title    = element_text(face="bold", size=15),
        plot.subtitle = element_text(colour="grey30", size=10.5),
        plot.caption  = element_text(colour="grey45", size=9, hjust=0),
        legend.position="top", panel.grid.minor=element_blank())

# PNG, explicitly opaque
png(file.path(graphs_dir(), with_date_prefix("t3_tau_over_time.png")),
    width=2200, height=1300, res=180, bg="white"); print(p); dev.off()
# PDF for the paper
pdf(file.path(graphs_dir(), with_date_prefix("t3_tau_over_time.pdf")),
    width=12.2, height=7.2); print(p); dev.off()
cat("wrote PNG and PDF\n")
cat(sprintf("  tau (binding) 1980 %.3f -> 2024 %.3f  (%.0f%%)\n",
    t[YEAR==1980]$tau_binding, t[YEAR==2024]$tau_binding,
    100*(t[YEAR==2024]$tau_binding/t[YEAR==1980]$tau_binding-1)))
