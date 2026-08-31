# =============================================================================
# T3 — presentation figures
#
# Five figures. Two conventions used throughout, both of which matter for how
# the evidence reads:
#
# 1. DECENNIAL YEARS (1980, 1990, 2000) ARE MARKED DISTINCTLY. They are a
#    different sample design from the 2001-2024 ACS, and model fit differs
#    across that boundary, so they must not be read as part of one continuous
#    series.
# 2. TARGETED vs UNTARGETED moments are visually separated. The untargeted ones
#    were never fitted in any year and are the model's real out-of-sample test;
#    presenting them alongside the fitted moments without distinction would
#    overstate what the fit demonstrates.
# =============================================================================
suppressMessages({library(data.table); library(ggplot2)})
source("functions.R"); source("R/paths.R")
results_dir <- data_path("processed", "results")
newest <- function(pat) {
  f <- sort(list.files(results_dir, pat, full.names = TRUE)); fread(f[length(f)])
}
est <- newest("t3_estimates_v2_by_year.csv$")
tau <- newest("t3_tau_series.csv$")
agg <- newest("t3_aggregate_distortion.csv$")
d   <- merge(merge(est, tau[, .(YEAR, tau_model, tau_binding, pct_binding)], by = "YEAR"),
             agg[, .(YEAR, pct_female_hours_lost, lost_per_affected, fte_lost_millions,
                     hours_lost_total, hours_gain_total)], by = "YEAR")
d[, era := factor(ifelse(YEAR %in% c(1980, 1990, 2000), "Decennial census", "ACS"),
                  levels = c("Decennial census", "ACS"))]

base_theme <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(colour = "grey30", size = 11),
        plot.caption = element_text(colour = "grey45", size = 9, hjust = 0),
        legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

# ── 1. tau over time: the headline ──────────────────────────────────────────
save_plot("t3_tau_over_time.png", {
  pd <- melt(d[, .(YEAR, era,
                   `All households` = tau_model,
                   `Households the norm binds on` = tau_binding)],
             id.vars = c("YEAR", "era"), variable.name = "measure", value.name = "tau")
  print(ggplot(pd, aes(YEAR, tau, colour = measure)) +
    geom_line(linewidth = 0.9) +
    geom_point(aes(shape = era), size = 2.6, fill = "white", stroke = 1) +
    scale_shape_manual(values = c("Decennial census" = 21, "ACS" = 19)) +
    scale_colour_manual(values = c("All households" = "#08519C",
                                   "Households the norm binds on" = "#B2182B")) +
    scale_y_continuous(limits = c(0, NA), labels = scales::percent_format(accuracy = 1)) +
    labs(title = expression(paste("The norm wedge ", tau, " = ", alpha, "C, 1980-2024")),
         subtitle = paste0("The implicit tax the breadwinner norm places on the wife's marginal earnings.\n",
                           "Reported as tau rather than alpha: alpha falls 89% over this period, but most of that\n",
                           "is nominal income quadrupling, not the norm weakening."),
         x = NULL, y = "Implicit tax on her earnings", colour = NULL, shape = "Sample",
         caption = "Source: IPUMS USA 1980-2024, 16.9M married couples. Structural estimates, one per year.") +
    base_theme)
}, width = 2200, height = 1300)

# ── 2. model vs data: targeted and untargeted ───────────────────────────────
save_plot("t3_model_vs_data_over_time.png", {
  mk <- function(dv, mv, lab, grp) data.table(YEAR = d$YEAR, era = d$era,
        Data = d[[dv]], Model = d[[mv]], moment = lab, grp = grp)
  pd <- rbindlist(list(
    mk("data_cliff",  "model_cliff",  "Cliff ratio (bunching below 0.5)", "TARGETED — fitted"),
    mk("data_corner", "model_corner", "Corner share (wife not working)",  "TARGETED — fitted"),
    mk("data_hshare", "model_hshare", "Wife's share of couple hours",     "UNTARGETED — never fitted"),
    mk("data_outearn","model_outearn","Share where wife out-earns",       "UNTARGETED — never fitted")))
  pd <- melt(pd, id.vars = c("YEAR","era","moment","grp"),
             variable.name = "src", value.name = "v")
  pd[, moment := factor(moment, levels = unique(moment))]
  print(ggplot(pd, aes(YEAR, v, colour = src, linetype = src)) +
    geom_line(linewidth = 0.85) + geom_point(size = 1.5) +
    facet_wrap(~ moment + grp, scales = "free_y", ncol = 2,
               labeller = labeller(.multi_line = TRUE)) +
    scale_colour_manual(values = c(Data = "#111111", Model = "#B2182B")) +
    scale_linetype_manual(values = c(Data = "solid", Model = "22")) +
    labs(title = "Model against data, every year 1980-2024",
         subtitle = paste0("Top row: the two moments the model was fitted to.  Bottom row: two moments held back entirely.\n",
                           "The untargeted moments track the data to within 0.017 and 0.014 on average across 27 years."),
         x = NULL, y = NULL, colour = NULL, linetype = NULL,
         caption = "Two free parameters (alpha, f) estimated separately each year against five targeted moments.") +
    base_theme)
}, width = 2400, height = 1500)

# ── 3. the offsetting forces ────────────────────────────────────────────────
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
    labs(title = "The norm's aggregate bite rose to 2006, then fell back",
         subtitle = paste0("Indexed to 1980 = 100. Two phases. To ~2006: exposure rises fast as wives' wages converge, while\n",
                           "intensity per affected household stays FLAT -- so the aggregate RISES 27%. After 2006: exposure\n",
                           "plateaus and intensity falls -- so the aggregate falls back. The endpoints (-2%) hide both moves."),
         x = NULL, y = "Index, 1980 = 100", colour = NULL,
         caption = paste0("Intensity is flat 1980-2018 (361 vs 362 hrs); its entire decline is 2019-2024, the years of ",
                          "worst model fit (loss 0.42 vs 0.34).")) +
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
    scale_shape_manual(values = c("Decennial census" = 21, "ACS" = 19)) +
    facet_wrap(~p, scales = "free_y", ncol = 2) +
    expand_limits(y = 0) +
    labs(title = "The norm's aggregate cost: a hump, peaking in 2006",
         subtitle = paste0("Counterfactual: each year re-solved with alpha = 0, holding the fixed cost and preferences fixed.\n",
                           "6.12% of women's market hours in 1980, peaking at 7.76% in 2006, back to 6.02% by 2024.\n",
                           "The absolute number still more than doubles, because population and exposure both grew."),
         x = NULL, y = NULL, shape = "Sample",
         caption = "The distortion is purely intensive: switching the norm off moves nobody into or out of the labour force.") +
    base_theme)
}, width = 2400, height = 1250)

# ── 5. where the model fails, shown honestly ────────────────────────────────
save_plot("t3_corner_gradient_limitation.png", {
  pd <- rbindlist(lapply(c("Q1","Q3","Q5"), function(q)
    rbindlist(list(
      data.table(YEAR=d$YEAR, era=d$era, q=q, src="Data",  v=d[[paste0("data_corner",q)]]),
      data.table(YEAR=d$YEAR, era=d$era, q=q, src="Model", v=d[[paste0("model_corner",q)]])))))
  pd[, q := factor(q, levels=c("Q1","Q3","Q5"),
        labels=c("Q1 — lowest-earning husbands","Q3 — middle","Q5 — highest-earning husbands"))]
  print(ggplot(pd, aes(YEAR, v, colour = src, linetype = src)) +
    geom_line(linewidth = 0.85) + geom_point(size = 1.4) +
    facet_wrap(~q, ncol = 3) +
    scale_colour_manual(values = c(Data = "#111111", Model = "#B2182B")) +
    scale_linetype_manual(values = c(Data = "solid", Model = "22")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "The model's known limitation: it puts non-participation in the wrong households",
         subtitle = paste0("Share of wives not working, by the husband's wage quintile. The model matches the AGGREGATE\n",
                           "corner share almost exactly, but assigns it by the income effect — too few non-workers among\n",
                           "low-earning husbands, too many among high-earning ones. In the data his wage barely predicts it."),
         x = NULL, y = "Wives not working", colour = NULL, linetype = NULL,
         caption = paste0("Tested and rejected as explanations: wage selection (alpha moves 18% across an extreme range) and ",
                          "preference heterogeneity\n(requires implausible dispersion and breaks the hours share). ",
                          "Disclosed rather than fitted around; no claim in Part 3 runs through this margin.")) +
    base_theme)
}, width = 2500, height = 1150)

message("wrote 5 T3 figures to data/graphs/")
