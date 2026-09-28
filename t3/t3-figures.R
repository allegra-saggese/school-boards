# =============================================================================
# T3 — all figures
#
# Inputs : data/processed/results/*_t3_estimates_v2_by_year.csv
#          data/processed/results/*_t3_tau_series.csv
#          data/processed/results/*_t3_aggregate_distortion.csv
#          data/processed/panel/model_input_households.csv
# Outputs: data/graphs/YYYY-MM-DD_t3_*.png  (figure 1 also as .pdf)
#
# Two conventions apply throughout:
#   1. Decennial years (1980, 1990, 2000) are marked with hollow points. They
#      are a different sample design from the 2001-2024 ACS and must not be
#      read as one continuous series.
#   2. Targeted and untargeted moments are labelled separately. The untargeted
#      ones were never fitted, and are the model's out-of-sample test.
#   3. Figures carry only a title, axis labels and a legend. What each one
#      shows, how it is computed and how to read it is documented per file in
#      notes/t3-figures-and-tables.md -- update that file when a figure changes.
# =============================================================================
suppressMessages({library(data.table); library(ggplot2)})
source(here::here("_setup.R"))

results_dir <- data_path("processed", "results")

est <- read_newest(results_dir, "t3_estimates_v2_by_year.csv$")
tau <- read_newest(results_dir, "t3_tau_series.csv$")
agg <- read_newest(results_dir, "t3_aggregate_distortion.csv$")

d <- merge(merge(est, tau[, .(YEAR, tau_model, tau_binding, pct_binding)], by = "YEAR"),
           agg[, .(YEAR, pct_female_hours_lost, lost_per_affected, fte_lost_millions,
                   hours_lost_total, hours_gain_total)], by = "YEAR")
d[, era := factor(ifelse(YEAR %in% c(1980, 1990, 2000), "Decennial census", "ACS"),
                  levels = c("Decennial census", "ACS"))]

era_shapes <- scale_shape_manual(values = c("Decennial census" = 21, "ACS" = 19))

base_theme <- theme_minimal(base_size = 13) +
  theme(plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.title    = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(colour = "grey30", size = 11),
        plot.caption  = element_text(colour = "grey45", size = 9, hjust = 0),
        legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

# ── 1. the norm wedge tau over time: the headline ───────────────────────────
# Plain-text title rather than plotmath, so the PDF embeds cleanly in LaTeX.
save_plot("t3_tau_over_time.png", {
  pd <- melt(d[, .(YEAR, era,
                   `All households` = tau_model,
                   `Households the norm binds on` = tau_binding)],
             id.vars = c("YEAR", "era"), variable.name = "measure", value.name = "tau")
  print(ggplot(pd, aes(YEAR, tau, colour = measure)) +
    geom_line(linewidth = 0.95) +
    geom_point(aes(shape = era), size = 2.5, fill = "white", stroke = 1) +
    era_shapes +
    scale_colour_manual(values = c("All households" = "#08519C",
                                   "Households the norm binds on" = "#B2182B")) +
    scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(round(100 * x), "%")) +
    scale_x_continuous(breaks = seq(1980, 2020, 10)) +
    labs(title = "The norm wedge tau, 1980-2024",
         x = "Year", y = "tau: implicit tax on her marginal earnings",
         colour = NULL, shape = "Sample") +
    base_theme)
}, width = 2200, height = 1300, also_pdf = TRUE)

# ── 2. model vs data: targeted and untargeted moments ───────────────────────
save_plot("t3_model_vs_data_over_time.png", {
  mk <- function(dv, mv, lab, grp) data.table(YEAR = d$YEAR, era = d$era,
        Data = d[[dv]], Model = d[[mv]], moment = lab, grp = grp)
  pd <- rbindlist(list(
    mk("data_cliff",  "model_cliff",  "Cliff ratio (bunching below 0.5)", "targeted"),
    mk("data_corner", "model_corner", "Corner share (wife not working)",  "targeted"),
    mk("data_hshare", "model_hshare", "Wife's share of couple hours",     "untargeted"),
    mk("data_outearn","model_outearn","Share where wife out-earns",       "untargeted")))
  pd <- melt(pd, id.vars = c("YEAR","era","moment","grp"),
             variable.name = "src", value.name = "v")
  pd[, moment := factor(moment, levels = unique(moment))]
  print(ggplot(pd, aes(YEAR, v, colour = src, linetype = src)) +
    geom_line(linewidth = 0.85) + geom_point(size = 1.5) +
    facet_wrap(~ moment + grp, scales = "free_y", ncol = 2,
               labeller = labeller(.multi_line = TRUE)) +
    scale_colour_manual(values = c(Data = "#111111", Model = "#B2182B")) +
    scale_linetype_manual(values = c(Data = "solid", Model = "22")) +
    labs(title = "Model vs data, 1980-2024",
         x = "Year", y = NULL, colour = NULL, linetype = NULL) +
    base_theme)
}, width = 2400, height = 1500)

# ── 3. the offsetting forces: intensity vs exposure ─────────────────────────
save_plot("t3_intensity_vs_exposure.png", {
  b <- d[YEAR == 1980]
  pd <- rbindlist(list(
    data.table(YEAR = d$YEAR, era = d$era, v = 100*d$lost_per_affected/b$lost_per_affected,
               s = "Intensity: hours lost per affected household"),
    data.table(YEAR = d$YEAR, era = d$era, v = 100*d$pct_binding/b$pct_binding,
               s = "Exposure: share of households the norm binds on"),
    data.table(YEAR = d$YEAR, era = d$era,
               v = 100*d$pct_female_hours_lost/b$pct_female_hours_lost,
               s = "NET: share of all female hours lost")))
  pd[, s := factor(s, levels = unique(s))]
  print(ggplot(pd, aes(YEAR, v, colour = s)) +
    geom_hline(yintercept = 100, colour = "grey55", linewidth = 0.4) +
    geom_line(linewidth = 1.0) + geom_point(size = 1.8) +
    scale_colour_manual(values = c("#2166AC", "#B2182B", "#111111")) +
    labs(title = "Intensity vs exposure of the norm, 1980-2024",
         x = "Year", y = "Index, 1980 = 100", colour = NULL) +
    base_theme + theme(legend.direction = "vertical"))
}, width = 2200, height = 1350)

# ── 4. the aggregate distortion ─────────────────────────────────────────────
save_plot("t3_aggregate_distortion.png", {
  pd <- rbindlist(list(
    data.table(YEAR=d$YEAR, era=d$era, v=d$pct_female_hours_lost,
               p="Share of female market hours lost to the norm (%)"),
    data.table(YEAR=d$YEAR, era=d$era, v=d$fte_lost_millions,
               p="Full-time-equivalent jobs lost (millions)")))
  pd[, p := factor(p, levels = unique(p))]
  print(ggplot(pd, aes(YEAR, v)) +
    geom_line(colour = "#08519C", linewidth = 0.95) +
    geom_point(aes(shape = era), colour = "#08519C", size = 2.4, fill = "white", stroke = 1) +
    era_shapes +
    facet_wrap(~p, scales = "free_y", ncol = 2) +
    expand_limits(y = 0) +
    labs(title = "Hours lost to the norm, 1980-2024",
         x = "Year", y = NULL, shape = "Sample") +
    base_theme)
}, width = 2400, height = 1250)

# ── 5. where the model fails, shown honestly ────────────────────────────────
save_plot("t3_corner_gradient_limitation.png", {
  pd <- rbindlist(lapply(c("Q1","Q3","Q5"), function(q)
    rbindlist(list(
      data.table(YEAR=d$YEAR, era=d$era, q=q, src="Data",  v=d[[paste0("data_corner",q)]]),
      data.table(YEAR=d$YEAR, era=d$era, q=q, src="Model", v=d[[paste0("model_corner",q)]])))))
  pd[, q := factor(q, levels=c("Q1","Q3","Q5"),
        labels=c("Q1 (lowest)","Q3","Q5 (highest)"))]
  print(ggplot(pd, aes(YEAR, v, colour = src, linetype = src)) +
    geom_line(linewidth = 0.85) + geom_point(size = 1.4) +
    facet_wrap(~q, ncol = 3) +
    scale_colour_manual(values = c(Data = "#111111", Model = "#B2182B")) +
    scale_linetype_manual(values = c(Data = "solid", Model = "22")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Wife not working, by husband's wage quintile",
         x = "Year", y = "Share of wives not working", colour = NULL, linetype = NULL) +
    base_theme)
}, width = 2500, height = 1150)

# ── 6. within-couple hours, earnings and wages ──────────────────────────────
# The flat male line is the point: the norm's threshold IS his earnings, so his
# stagnation is why more couples now hit the constraint.
hh <- fread(data_path("processed","panel","model_input_households.csv"),
            select = c("YEAR","HHWT","f_h","m_h","f_lab","m_lab","f_w","m_w",
                       "f_w_predicted","m_w_predicted"), showProgress = FALSE)
wq <- function(x, w, p = .5) { i <- order(x); x <- x[i]; w <- w[i]
                               x[which.max(cumsum(w)/sum(w) >= p)] }

hrs <- hh[, .(Wife = weighted.mean(f_h, HHWT), Husband = weighted.mean(m_h, HHWT)), by = YEAR]
ern <- hh[, .(Wife = weighted.mean(deflate_to(f_lab, YEAR, 2024), HHWT),
              Husband = weighted.mean(deflate_to(m_lab, YEAR, 2024), HHWT)), by = YEAR]
wg  <- hh[m_w_predicted == FALSE & f_w_predicted == FALSE,
          .(Wife = wq(deflate_to(f_w, YEAR, 2024), HHWT),
            Husband = wq(deflate_to(m_w, YEAR, 2024), HHWT)), by = YEAR]

save_plot("t3_hours_earnings_wife_vs_husband.png", {
  mk <- function(x, lab) melt(x, id.vars = "YEAR", variable.name = "spouse",
                              value.name = "v")[, panel := lab][]
  pd <- rbindlist(list(mk(hrs, "Annual market hours"),
                       mk(ern, "Annual labour earnings (2024 $)"),
                       mk(wg,  "Median hourly wage (2024 $)")))
  pd[, panel := factor(panel, levels = c("Annual market hours",
                                         "Annual labour earnings (2024 $)",
                                         "Median hourly wage (2024 $)"))]
  pd[, era := ifelse(YEAR %in% c(1980, 1990, 2000), "Decennial census", "ACS")]
  print(ggplot(pd, aes(YEAR, v, colour = spouse)) +
    geom_line(linewidth = 1.0) +
    geom_point(aes(shape = era), size = 2.2, fill = "white", stroke = 0.9) +
    era_shapes +
    scale_colour_manual(values = c(Wife = "#B2182B", Husband = "#08519C")) +
    facet_wrap(~panel, scales = "free_y", ncol = 3) +
    expand_limits(y = 0) +
    labs(title = "Wife vs husband: hours, earnings and wages, 1980-2024",
         x = "Year", y = NULL, colour = NULL, shape = "Sample") +
    base_theme + theme(panel.spacing = unit(1.4, "lines")))
}, width = 2600, height = 1150)

# ── 7. untargeted intensive-margin tests by husband's-wage quintile ─────────
# Group (C) of the moment report. Each gradient exists MECHANICALLY at
# alpha = 0 (wage composition: high-wage husbands out-earn their wives), so the
# raw Q1 -> Q5 slope is not evidence. The test is where the DATA sit relative
# to the MODEL and the NO-NORM baseline, the same model with alpha = 0 and F,
# kappa unchanged. Pooled over the ACS years: lines are the mean across years,
# bands the min-max range across years. Decennial years are excluded from the
# pooling (different sample design; convention 1 above).
save_plot("t3_untargeted_by_quintile.png", {
  acs <- d[era == "ACS"]
  specs <- list(
    cliff_Q    = "Cliff ratio within quintile\n(mass just below 0.5 / just above)",
    overhrs_Q  = "Dual earners: share where\nshe works more hours than he does",
    hshareDE_Q = "Dual earners: her share\nof the couple's hours")
  pd <- rbindlist(lapply(names(specs), function(nm)
    rbindlist(lapply(1:5, function(q)
      rbindlist(lapply(c(Data = "data_", Model = "model_", `No norm (alpha = 0)` = "nonorm_"),
        function(pfx) { v <- acs[[paste0(pfx, nm, q)]]
          data.table(mean = mean(v, na.rm = TRUE), lo = min(v, na.rm = TRUE),
                     hi = max(v, na.rm = TRUE)) }), idcol = "src")[, q := q]))[,
      panel := specs[[nm]]]))
  pd[, panel := factor(panel, levels = unlist(specs))]
  pd[, src := factor(src, levels = c("Data", "Model", "No norm (alpha = 0)"))]
  cols <- c(Data = "#111111", Model = "#B2182B", `No norm (alpha = 0)` = "#6BAED6")
  # One panel per moment, assembled with patchwork, so that the cliff panel
  # alone can take a LOG scale: the model's Q5 cliff is an order of magnitude
  # above the data and on a linear axis it flattens everything else.
  panel_plot <- function(lab, ylab, log_y = FALSE) {
    g <- ggplot(pd[panel == lab], aes(q, mean, colour = src, fill = src)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) +
      geom_line(aes(linetype = src), linewidth = 0.95) + geom_point(size = 2) +
      scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
      scale_linetype_manual(values = c(Data = "solid", Model = "22", `No norm (alpha = 0)` = "solid")) +
      scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
      labs(subtitle = lab, x = "Husband's wage quintile", y = ylab,
           colour = NULL, fill = NULL, linetype = NULL) +
      base_theme + theme(plot.subtitle = element_text(face = "bold", colour = "black", size = 12))
    if (log_y) g + scale_y_log10() else g
  }
  print(patchwork::wrap_plots(
          panel_plot(specs$cliff_Q,    "Ratio (log scale)", log_y = TRUE),
          panel_plot(specs$overhrs_Q,  "Share of couples"),
          panel_plot(specs$hshareDE_Q, "Her share of hours"), ncol = 3) +
        patchwork::plot_layout(guides = "collect") +
        patchwork::plot_annotation(title = "Untargeted tests by husband's wage quintile",
                                   theme = base_theme))
}, width = 2600, height = 1150)

# ── console summary ─────────────────────────────────────────────────────────
cat(sprintf("\ntau (binding) 1980 %.3f -> 2024 %.3f  (%+.0f%%)\n",
    tau[YEAR == 1980]$tau_binding, tau[YEAR == 2024]$tau_binding,
    100 * (tau[YEAR == 2024]$tau_binding / tau[YEAR == 1980]$tau_binding - 1)))
cat("1980 vs 2024, indexed:\n")
for (nm in c("hrs", "ern", "wg")) {
  x <- get(nm); b <- x[YEAR == 1980]; e <- x[YEAR == 2024]
  cat(sprintf("  %-28s wife %+5.0f%%   husband %+5.0f%%\n",
      c(hrs = "annual hours", ern = "annual earnings (real)",
        wg = "hourly wage (real)")[nm],
      100 * (e$Wife / b$Wife - 1), 100 * (e$Husband / b$Husband - 1)))
}
message("wrote 7 T3 figures to data/graphs/")
