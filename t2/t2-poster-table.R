# =============================================================================
# T2 — short regression table for the poster
#
# Input  : data/processed/results/*_t2_regressions.csv   (t2-empirical-quadrant.R)
# Output : data/processed/results/YYYY-MM-DD_t2_poster_table.tex / .csv
#
# Three of the six columns of t2_main_table.tex: the preferred participation
# specification (top-quartile home value), the outright-ownership robustness, and
# hours. Same models, same coefficients, same clustered standard errors; only the
# selection and the formatting differ. Participation is in percentage points,
# hours in weekly hours.
# =============================================================================
suppressMessages(library(data.table))
source(here::here("_setup.R"))
results_dir <- data_path("processed", "results")
rg <- read_newest(results_dir, "t2_regressions.csv$")

stars <- function(p) ifelse(p < 0.01, "$^{***}$", ifelse(p < 0.05, "$^{**}$", ifelse(p < 0.1, "$^{*}$", "")))
specs <- list(
  list(col = "LFP (pp)",       model = "LFP top-quartile home",   wealth = "wealthy",  scale = 100),
  list(col = "LFP (pp), alt.", model = "LFP owns outright",       wealth = "outright", scale = 100),
  list(col = "Weekly hours",   model = "Hours top-quartile home", wealth = "wealthy",  scale = 1))
pick <- function(s, kind) {
  want <- switch(kind, county = "conservative", wealth = s$wealth, inter = paste0("conservative:", s$wealth))
  r <- rg[model == s$model & term == want]
  stopifnot(nrow(r) == 1)
  list(est = paste0(formatC(s$scale * r$estimate,  format = "f", digits = 2), stars(r$p_value)),
       se  = paste0("(", formatC(s$scale * r$std_error, format = "f", digits = 2), ")"))
}
mk <- function(label, kind) {
  p <- lapply(specs, pick, kind = kind)
  out <- data.table(Term = c(label, ""))
  for (i in seq_along(specs)) out[[specs[[i]]$col]] <- c(p[[i]]$est, p[[i]]$se)
  out
}
tab <- rbind(
  data.table(Term = "Wealth measured as", `LFP (pp)` = "Top-quartile home", `LFP (pp), alt.` = "Owns outright", `Weekly hours` = "Top-quartile home"),
  mk("Republican-majority county", "county"),
  mk("Wealth measure",             "wealth"),
  mk("Republican $\\times$ wealth", "inter"),
  data.table(Term = c("Controls", "State, year, husband-decile FE", "Couples"),
             `LFP (pp)` = c("Yes", "Yes", "2,477,474"), `LFP (pp), alt.` = c("Yes", "Yes", "2,477,474"),
             `Weekly hours` = c("Yes", "Yes", "2,477,474")))
fwrite(tab, dated_path(results_dir, "t2_poster_table.csv"))
write_tex_table(tab, dated_path(results_dir, "t2_poster_table.tex"),
  groups = list("Effect on the wife's labor supply" = nrow(tab) - 3, "Specification" = 3),
  raw_cols = c("Term", "LFP (pp)", "LFP (pp), alt.", "Weekly hours"),
  notes = "Standard errors clustered by county in parentheses. $^{***}$ p$<$0.01, $^{**}$ p$<$0.05, $^{*}$ p$<$0.1. Controls: ages, college, children, ln husband labor income.")
print(tab)
