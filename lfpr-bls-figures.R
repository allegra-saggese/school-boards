# Simple BLS labour force participation rate plots, no titles or subtitles.
#   LNS11300001  Men,   16+, monthly, seasonally adjusted
#   LNS11300002  Women, 16+, monthly, seasonally adjusted
# Source: FRED / BLS.
suppressMessages({library(data.table); library(ggplot2); library(readxl)})
source("functions.R"); source("R/paths.R")

men <- as.data.table(read_excel("~/Downloads/LNS11300001.xlsx", sheet = "Monthly"))
setnames(men, c("date", "lfpr")); men[, sex := "Men"]
men[, date := as.Date(date)]   # readxl returns POSIXct; fread returns IDate
wom <- fread("~/Downloads/LNS11300002.csv")
setnames(wom, c("date", "lfpr")); wom[, sex := "Women"]
wom[, date := as.Date(date)]

d <- rbind(men, wom)
d[, lfpr := as.numeric(lfpr)]
d <- d[!is.na(lfpr) & !is.na(date)]          # Oct 2025 is missing in the women's series

bare <- theme_minimal(base_size = 14) +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.caption = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12), legend.position = "none")

one <- function(sx, col, file) {
  save_plot(file, {
    print(ggplot(d[sex == sx], aes(date, lfpr)) +
      geom_line(colour = col, linewidth = 0.8) +
      scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = NULL, y = "Labor force participation rate") + bare)
  }, width = 1800, height = 1000)
}
one("Men",   "#08519C", "bls_lfpr_men.png")
one("Women", "#B2182B", "bls_lfpr_women.png")

# both together, since the comparison is the point
save_plot("bls_lfpr_men_women.png", {
  print(ggplot(d, aes(date, lfpr, colour = sex)) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = c(Men = "#08519C", Women = "#B2182B")) +
    scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = "Labor force participation rate", colour = NULL) +
    bare + theme(legend.position = "top"))
}, width = 1800, height = 1000)

cat(sprintf("Men   %s to %s: %.1f%% -> %.1f%%  (peak %.1f%% in %s)\n",
    format(min(d[sex=="Men"]$date),"%Y"), format(max(d[sex=="Men"]$date),"%Y"),
    d[sex=="Men"][1]$lfpr, d[sex=="Men"][.N]$lfpr,
    max(d[sex=="Men"]$lfpr), format(d[sex=="Men"][which.max(lfpr)]$date,"%Y")))
cat(sprintf("Women %s to %s: %.1f%% -> %.1f%%  (peak %.1f%% in %s)\n",
    format(min(d[sex=="Women"]$date),"%Y"), format(max(d[sex=="Women"]$date),"%Y"),
    d[sex=="Women"][1]$lfpr, d[sex=="Women"][.N]$lfpr,
    max(d[sex=="Women"]$lfpr), format(d[sex=="Women"][which.max(lfpr)]$date,"%Y")))
