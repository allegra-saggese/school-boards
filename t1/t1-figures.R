# =============================================================================
# T1 — all figures
#
# Inputs : data/processed/panel/model_input_households.csv
#          data/processed/results/*_bkp_pure_cliff_ratio_by_year.csv
#          data/processed/results/*_bkp_pure_beta1_by_year.csv
#          data/processed/results/*_bkp_pure_table23_era_comparison.csv
# Outputs: data/graphs/YYYY-MM-DD_bkp_pure_*.png
#          data/processed/results/YYYY-MM-DD_bkp_pure_descriptive_trends.csv
#
# Drawing only. Run t1-replication.R and t1-summary-outputs.R first; this reads
# their CSVs so a figure can be redrawn without refitting anything.
# =============================================================================
suppressMessages({library(data.table); library(ggplot2)})
source(here::here("_setup.R"))

results_dir <- data_path("processed", "results")

base_theme <- theme_minimal(base_size = 13) +
  theme(plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey30", size = 10.5),
        legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 11))

# ── 1. descriptive trends: the Data section ─────────────────────────────────
# The trends the paper is about, before any model or regression touches them.
d <- fread(data_path("processed", "panel", "model_input_households.csv"),
           select = c("YEAR","HHWT","f_h","m_h","f_lab","m_lab","f_emp","m_emp"),
           showProgress = FALSE)
d[, `:=`(works   = as.numeric(f_h > 0),
         zW      = fifelse((f_lab + m_lab) > 0, f_lab/(f_lab + m_lab), NA_real_),
         outearn = as.numeric(f_lab > m_lab))]
s <- d[, .(lfp     = 100*weighted.mean(works, HHWT),
           share   = weighted.mean(zW, HHWT, na.rm = TRUE),
           outearn = 100*weighted.mean(outearn, HHWT),
           hrs_gap = weighted.mean(m_h - f_h, HHWT)), by = YEAR][order(YEAR)]
s[, era := ifelse(YEAR %in% c(1980, 1990, 2000), "Decennial census", "ACS")]

save_plot("bkp_pure_descriptive_trends.png", {
  pd <- rbindlist(list(
    s[, .(YEAR, era, v = lfp,       panel = "(a) Wives with positive annual hours (%)")],
    s[, .(YEAR, era, v = 100*share, panel = "(b) Wife's share of couple labor earnings (%)")],
    s[, .(YEAR, era, v = outearn,   panel = "(c) Couples where the wife out-earns (%)")],
    s[, .(YEAR, era, v = hrs_gap,   panel = "(d) Husband minus wife annual hours")]))
  pd[, panel := factor(panel, levels = unique(panel))]
  print(ggplot(pd, aes(YEAR, v)) +
    geom_line(colour = "#08519C", linewidth = 0.9) +
    geom_point(aes(shape = era), colour = "#08519C", size = 2.1, fill = "white", stroke = 0.9) +
    scale_shape_manual(values = c("Decennial census" = 21, "ACS" = 19)) +
    facet_wrap(~panel, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = NULL, shape = "Sample") +
    base_theme + theme(panel.spacing = unit(1.2, "lines")))
}, width = 2300, height = 1400)

fwrite(s, dated_path(results_dir, "bkp_pure_descriptive_trends.csv"))

cat("Key descriptive series, married couples both 18-65\n\n")
cat(sprintf("%-6s %10s %12s %12s %10s\n", "year", "wife works", "wife share",
            "out-earns", "hours gap"))
for (y in c(1980, 1990, 2000, 2010, 2020, 2024)) {
  r <- s[YEAR == y]
  cat(sprintf("%-6d %9.1f%% %11.3f %11.1f%% %10.0f\n",
              y, r$lfp, r$share, r$outearn, r$hrs_gap))
}
b <- s[YEAR == 1980]; e <- s[YEAR == 2024]
cat(sprintf("\n1980->2024:  wife works %+.1f pp | share %+.3f | out-earns %+.1f pp | gap %+.0f hrs\n",
    e$lfp - b$lfp, e$share - b$share, e$outearn - b$outearn, e$hrs_gap - b$hrs_gap))

# ── 2. cliff ratio by year: has the threshold signature weakened? ───────────
cliff_yr <- read_newest(results_dir, "bkp_pure_cliff_ratio_by_year.csv$")
save_plot("bkp_pure_cliff_ratio_by_year.png", {
  print(ggplot(cliff_yr, aes(x = YEAR, y = cliff)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.18) +
    geom_line(colour = "steelblue4", linewidth = 0.9) +
    geom_point(colour = "steelblue4", size = 1.8) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_vline(xintercept = 2011.5, linetype = "dotted", colour = "grey30") +
    annotate("text", x = 2012.5, y = max(cliff_yr$hi, na.rm = TRUE),
             label = "BKP sample ends", hjust = 0, size = 3, colour = "grey30") +
    labs(title = "Threshold avoidance over time",
         subtitle = paste0("Couples just below the equal-earnings threshold relative to just above ",
                           "([0.40,0.48) vs (0.52,0.60], ±2pp donut excluded).\n",
                           "1.0 = no avoidance. Shaded band = bootstrap 95% CI."),
         x = NULL, y = "Below / above ratio") +
    base_theme)
}, width = 2000, height = 1200)

# ── 3. beta1 across samples and specifications ──────────────────────────────
# Reads the coefficients the replication already produced rather than refitting.
coef_f <- list.files(results_dir, "bkp_pure_table23_era_comparison.csv$", full.names = TRUE)
if (length(coef_f)) {
  cf <- read_newest(results_dir, "bkp_pure_table23_era_comparison.csv$")
  cf[, `:=`(lo = beta1 - 1.96*se, hi = beta1 + 1.96*se)]
  cf[, spec := fcase(grepl("col 1", outcome), "Col 1: linear",
                     grepl("col 2", outcome), "Col 2: cubic",
                     grepl("col 4", outcome), "Col 4: cubic + children",
                     default = "other")]
  cf[, dv := fifelse(grepl("Wife LFP", outcome), "Wife's LFP", "Income gap")]
  cf[, era_short := fcase(grepl("BKP era", era),  "(a) Replication\n1970-2011",
                          grepl("Post-BKP", era), "(b) Post-BKP\n2012-2024",
                          grepl("UPDATED", era),  "(c) Updated\n1970-2024", default = era)]
  # BKP's published values, as reference lines
  bkp_ref <- data.table(dv = rep(c("Wife's LFP", "Income gap"), each = 3),
                        spec = rep(c("Col 1: linear", "Col 2: cubic",
                                     "Col 4: cubic + children"), 2),
                        published = c(-0.178, -0.142, -0.143, -0.031, -0.095, -0.109))
  cf <- merge(cf, bkp_ref, by = c("dv", "spec"), all.x = TRUE)

  save_plot("bkp_pure_coefficient_plot.png", {
    print(ggplot(cf, aes(x = era_short, y = beta1, colour = spec)) +
      geom_hline(yintercept = 0, colour = "grey60") +
      geom_hline(aes(yintercept = published, colour = spec), linetype = "dashed", alpha = 0.55) +
      geom_pointrange(aes(ymin = lo, ymax = hi),
                      position = position_dodge(width = 0.55), size = 0.55) +
      facet_wrap(~dv, scales = "free_y") +
      scale_colour_manual(values = c("Col 1: linear" = "#d73027",
                                     "Col 2: cubic" = "#4575b4",
                                     "Col 4: cubic + children" = "#1a9850"), name = NULL) +
      labs(title = "Effect of PrWifeEarnsMore, by sample and specification",
           subtitle = paste0("Points with 95% CIs. Dashed lines = BKP's published estimates.\n",
                             "The linear specification (red) does not replicate; the cubic does."),
           x = NULL, y = expression(beta[1])) +
      base_theme + theme(legend.position = "bottom"))
  }, width = 2200, height = 1200)
} else {
  message("skipped coefficient plot: run t1-replication.R first")
}

# ── 4. beta1 by year ────────────────────────────────────────────────────────
by_year <- read_newest(results_dir, "bkp_pure_beta1_by_year.csv$")
save_plot("bkp_pure_beta1_by_year.png", {
  print(ggplot(by_year, aes(x = YEAR, y = beta1)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#4575b4", alpha = 0.18) +
    geom_line(colour = "#4575b4", linewidth = 0.9) +
    geom_point(colour = "#4575b4", size = 1.8) +
    geom_hline(yintercept = 0, colour = "grey55") +
    geom_hline(yintercept = -0.142, linetype = "dashed", colour = "#d73027") +
    annotate("text", x = min(by_year$YEAR), y = -0.142, vjust = -0.6, hjust = 0,
             label = "BKP published (-0.142)", size = 3, colour = "#d73027") +
    geom_vline(xintercept = 2011.5, linetype = "dotted", colour = "grey30") +
    labs(title = "Effect of potential relative income on wife's participation, by year",
         subtitle = paste0("Separate regression each year, BKP's cubic specification. ",
                           "Shaded band = 95% CI.\nMore negative = stronger aversion to out-earning."),
         x = NULL, y = expression(beta[1]~" on PrWifeEarnsMore")) +
    base_theme)
}, width = 2100, height = 1200)

message("wrote 4 T1 figures to data/graphs/")
