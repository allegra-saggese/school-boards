# =============================================================================
# T3 — calibration comparison table (poster / paper)
#
# Inputs : data/processed/results/*_t3_bunching_fit_by_year_eps05_all.csv
#            bunching-calibrated fit, 27 years (t3-estimate-bunching.R)
#          data/processed/results/*_t3_bunching_fit_by_year_cliff_e05.csv
#            cliff-calibrated fit, 2019 only (T3B_TARGET=cliff)
#          data/processed/panel/model_input_households.csv
# Outputs: data/processed/results/YYYY-MM-DD_t3_calibration_table.tex / .csv
#
# One table, one year (2019), one elasticity (eps = 0.5, gamma = 1, theta = 1).
# The two calibrations differ ONLY in which moment pins down alpha:
#   cliff-calibrated    cliff ratio  + corner share
#   bunching-calibrated Saez bunching wedge + corner share
# Targeted moments are marked; every other row is a test the fit never saw.
# Panel A is model-only: it translates each calibrated alpha into hours, using
# the same solver and the same 150,000-couple sample for both.
# =============================================================================
suppressMessages(library(data.table))
source(here::here("_setup.R"))
source(here::here("t3", "t3-model-solver-eps.R"))

results_dir <- data_path("processed", "results")
yr  <- 2019L
eps <- 0.5
bu  <- read_newest(results_dir, "t3_bunching_fit_by_year_eps05_all.csv$")[YEAR == yr]
cl  <- read_newest(results_dir, "t3_bunching_fit_by_year_cliff_e05.csv$")[YEAR == yr]
stopifnot(nrow(bu) == 1, nrow(cl) == 1, cl$eps == eps)

# Panel A: translate alpha into hours on one fixed sample
d <- fread(file.path(data_path("processed", "panel"), "model_input_households.csv"),
           select = c("YEAR", "HHWT", "f_w", "m_w", "y0", "y", "f_h", "m_h"), showProgress = FALSE)[YEAR == yr]
d <- d[is.finite(f_w) & is.finite(m_w) & f_w > 0 & m_w > 0 & is.finite(y0) & is.finite(f_h) & is.finite(m_h)]
ymed <- median(d$y, na.rm = TRUE)
set.seed(1); d <- d[sample.int(.N, 150000)]
effect <- function(fit) {
  k  <- rep(fit$kappa, nrow(d)); F <- fit$f * ymed
  s1 <- solve_household_eps(d$m_w, d$f_w, d$y0, F, fit$alpha, k, k, eps = eps)
  s0 <- solve_household_eps(d$m_w, d$f_w, d$y0, F, 0,         k, k, eps = eps)
  w <- d$HHWT; bound <- s1$regime %in% c(2L, 3L)
  c(wedge = fit$tau_binding, pct_bound = 100 * weighted.mean(bound, w),
    hrs_lost = weighted.mean((s0$h_f - s1$h_f)[bound], w[bound]),
    pct_hours = 100 * sum(w * (s0$h_f - s1$h_f)) / sum(w * s0$h_f))
}
A_cl <- effect(cl); A_bu <- effect(bu)

f3 <- function(x, dg = 3) formatC(x, format = "f", digits = dg)
tg <- "\\textdagger{}"                     # marks a targeted moment
cell <- function(x, dg = 3, targeted = FALSE) paste0(f3(x, dg), if (targeted) tg else "")
mom <- function(nm, label, dg = 3, t_cl = FALSE, t_bu = FALSE)
  data.table(Moment = label, Data = f3(cl[[paste0("data_", nm)]], dg),
             `Cliff-calibrated` = cell(cl[[paste0("model_", nm)]], dg, t_cl),
             `Bunching-calibrated` = cell(bu[[paste0("model_", nm)]], dg, t_bu),
             `No norm` = f3(bu[[paste0("nonorm_", nm)]], dg))
dash <- function(label, a, b, dg, unit = "")
  data.table(Moment = label, Data = "--", `Cliff-calibrated` = paste0(f3(a, dg), unit),
             `Bunching-calibrated` = paste0(f3(b, dg), unit), `No norm` = "--")

tab <- rbind(
  dash("Binding wedge $\\tau$",                A_cl["wedge"],     A_bu["wedge"],     3),
  dash("Couples where the norm binds (\\%)",   A_cl["pct_bound"], A_bu["pct_bound"], 1),
  dash("Her hours lost per bound couple",      A_cl["hrs_lost"],  A_bu["hrs_lost"],  0),
  dash("Wives' market hours lost (\\%)",       A_cl["pct_hours"], A_bu["pct_hours"], 2),
  mom("tau_bar",  "Bunching wedge $\\hat\\tau$", 3, FALSE, TRUE),
  mom("cliff",    "Cliff ratio",                  3, TRUE,  FALSE),
  mom("corner",   "Wife not working",             3, TRUE,  TRUE),
  mom("hshare",   "Wife's share of couple hours", 3),
  mom("outearn",  "Wife out-earns husband",       3),
  mom("cornerQ1", "Wife not working, husband wage Q1", 3),
  mom("cornerQ5", "Wife not working, husband wage Q5", 3),
  mom("overhrsQ1","Dual earners, she works more hours: Q1", 3),
  mom("overhrsQ5","Dual earners, she works more hours: Q5", 3))
# tau-hat data value comes from the T2 bunching estimate, not the model column
tab[Moment == "Bunching wedge $\\hat\\tau$", Data := f3(cl$data_tau_bar)]
# "wife's hours lost" / "bunching" / "cliff" cells that are literally the targets
fwrite(tab, dated_path(results_dir, "t3_calibration_table.csv"))

# the dagger and the math in labels are LaTeX already
write_tex_table(tab, dated_path(results_dir, "t3_calibration_table.tex"),
  groups = list("A. What the norm does (model)" = 4,
                "B. Moments the calibrations target (\\textdagger{})" = 3,
                "C. Untargeted moments" = 6),
  raw_cols = c("Moment", "Cliff-calibrated", "Bunching-calibrated"),
  notes = paste0("2019; Frisch elasticity 0.5, log utility, symmetric $\\kappa$. \\textdagger{} = targeted by that calibration. ",
                 "The two calibrations differ only in which moment pins down $\\alpha$. ",
                 "No norm sets $\\alpha = 0$ at the bunching-calibrated $F$ and $\\kappa$. ",
                 "Wedge $\\tau = \\alpha C$ averaged over couples where the norm binds."))
print(tab)

# ── Short version: one calibration (cliff), data vs model only ───────────────
# Same numbers as the long table's Data and Cliff-calibrated columns, with short
# row labels and no no-norm column. The bunching wedge is UNTARGETED here, which
# is the point: the cliff calibration implies a wedge far above the data's.
srow <- function(label, d, m, dg = 3, targeted = FALSE)
  data.table(Moment = label, Data = d, Model = paste0(m, if (targeted) tg else ""))
sh <- rbind(
  srow("Binding wedge $\\tau$",          "--", f3(A_cl["wedge"], 3)),
  srow("Couples where it binds (\\%)",   "--", f3(A_cl["pct_bound"], 1)),
  srow("Her hours lost, bound couples",  "--", f3(A_cl["hrs_lost"], 0)),
  srow("Wives' hours lost (\\%)",        "--", f3(A_cl["pct_hours"], 1)),
  srow("Cliff ratio",                    f3(cl$data_cliff),   f3(cl$model_cliff),  targeted = TRUE),
  srow("Wife not working",               f3(cl$data_corner),  f3(cl$model_corner), targeted = TRUE),
  srow("Bunching wedge $\\hat\\tau$",    f3(cl$data_tau_bar), f3(cl$model_tau_bar)),
  srow("Wife's hours share",             f3(cl$data_hshare),  f3(cl$model_hshare)),
  srow("Wife out-earns",                 f3(cl$data_outearn), f3(cl$model_outearn)),
  srow("Not working, Q1",                f3(cl$data_cornerQ1),f3(cl$model_cornerQ1)),
  srow("Not working, Q5",                f3(cl$data_cornerQ5),f3(cl$model_cornerQ5)),
  srow("She works more hours, Q1",       f3(cl$data_overhrsQ1), f3(cl$model_overhrsQ1)),
  srow("She works more hours, Q5",       f3(cl$data_overhrsQ5), f3(cl$model_overhrsQ5)))
fwrite(sh, dated_path(results_dir, "t3_calibration_table_short.csv"))
write_tex_table(sh, dated_path(results_dir, "t3_calibration_table_short.tex"),
  groups = list("Norm (model)" = 4, "Targeted\\textdagger{}" = 2, "Untargeted" = 7),
  raw_cols = c("Moment", "Model"),
  notes = "2019, Frisch elasticity 0.5. \\textdagger{} targeted. Wedge $\\tau = \\alpha C$ among couples where the norm binds. Q = husband's wage quintile.")
