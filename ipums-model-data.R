library(data.table)
library(DBI)
library(RSQLite)

source("functions.R")
source("R/paths.R")

# =========================================================
# Model dataset: inputs for the household utility model
#
# Builds the household-level inputs the theory needs, from ipums_bkp.sqlite.
# The model (Saggese, May 2026 presentation, slides 34-40) is:
#
#   max  u(C) + v^m(L^m) + v^f(L^f) - alpha_eff * V(h_m, h_f)
#   s.t. C = w_m*h_m + w_f*h_f + y0 ,  h_i = T - L^i ,  h_i >= 0
#        V(h_m, h_f) = max(w_f*h_f - w_m*h_m, 0)          <- CONTINUOUS
#
# Each object below is built to match a specific role in that problem. The
# previous implementation used quantities that did not correspond to the theory;
# the differences are noted inline because they change the estimates materially.
#
#  w_j   PRICE of an hour of j's labour. Must (a) contain only LABOUR income --
#        capital income does not scale with hours -- and (b) not be constructed
#        from the household's own chosen hours where those hours are
#        uninformative, or the FOC that pins preferences becomes circular.
#        [previously: (INCTOT - INCSS - INCWELFR)/hours, which contains capital
#        income and inflates the implied wage for asset-holders]
#
#  y0    Income entering the budget that does NOT respond to hours: capital,
#        retirement, and transfer income.
#        [previously: a residual, hhincome - both spouses' income, which
#        double-counted -- capital income sat inside w*h AND was subtracted
#        from y0]
#
#  z_W   Wife's share of LABOUR earnings, matching the model's own definition
#        w_f*h_f / (w_m*h_m + w_f*h_f). No capital income.
#
#  h_j   Annual hours = usual weekly hours x weeks worked. Weeks come from
#        WKSWORK1, or the interval reconstruction for 2008-2018 (see
#        weeks_worked() in functions.R). UHRSWORK is used AS REPORTED and is
#        NOT zeroed by current EMPSTAT: UHRSWORK and income share the same
#        reference period (last year / past 12 months), whereas EMPSTAT is a
#        survey-week snapshot. Zeroing by EMPSTAT produces income with no hours.
#
# SAMPLE: all couples, INCLUDING h_f = 0 corners. The model permits h >= 0 and
# a corner is its sharpest prediction, so conditioning on h_f > 0 would select
# on the model's own dependent variable. Measured: 25% of couples are at the
# corner, and male-sole-earner couples outnumber female-sole-earner ones ~3.9:1
# -- that asymmetry IS the norm and is invisible in a dual-earner sample.
# =========================================================

model_years    <- 2010:2020   # calibration window
min_ann_hours  <- 260L        # 5 hrs/wk year-round: below this a wage is not measured
lump_income    <- 100000      # see "lump-sum exclusion" below
min_cell_n     <- 30L

results_dir <- data_path("processed", "results")
panel_dir   <- data_path("processed", "panel")
ensure_dir(results_dir); ensure_dir(panel_dir)

sqlite_path <- data_path("interim", "ipums_bkp.sqlite")
if (!file.exists(sqlite_path)) stop("Missing ", sqlite_path)
con <- dbConnect(SQLite(), sqlite_path)
on.exit(dbDisconnect(con), add = TRUE)

r_main <- setDT(dbGetQuery(con, "SELECT YEAR, MIN(rowid) lo, MAX(rowid) hi FROM ipums_table GROUP BY YEAR"))
r_wks  <- setDT(dbGetQuery(con, "SELECT YEAR, MIN(rowid) lo, MAX(rowid) hi FROM wks_supp   GROUP BY YEAR"))
setkey(r_main, YEAR); setkey(r_wks, YEAR)

# IPUMS income N/A sentinels are codes, not dollars.
nas <- function(x) fifelse(x >= 999999, NA_real_, as.numeric(x))

# BKP's labour-income concept: wage + self-employment, era-spliced.
labor_income <- function(w, b, f, b0) {
  w <- nas(w); b0 <- nas(b0); b <- nas(b); f <- nas(f)
  se <- fifelse(!is.na(b0), b0, rowSums(cbind(b, f), na.rm = TRUE))
  pmax(rowSums(cbind(w, se), na.rm = TRUE), 0)
}

pull_year <- function(yr) {
  a <- r_main[J(yr)]; b <- r_wks[J(yr)]
  main <- setDT(dbGetQuery(con, paste0(
    "SELECT SAMPLE,SERIAL,PERNUM,SEX,SPLOC,HHWT,AGE,RACE,HISPAN,EDUC,EMPSTAT,
            UHRSWORK,INCWAGE,INCBUS,INCFARM,INCBUS00,INCINVST,INCOTHER,
            INCSS,INCWELFR,HHINCOME,NCHILD,STATEICP,COUNTYICP
     FROM ipums_table NOT INDEXED
     WHERE rowid BETWEEN ", a$lo, " AND ", a$hi, " AND YEAR = ", yr,
    " AND SPLOC > 0 AND AGE BETWEEN 25 AND 64")))
  wk <- setDT(dbGetQuery(con, paste0(
    "SELECT SAMPLE,SERIAL,PERNUM,WKSWORK1,WKSWORK2 FROM wks_supp NOT INDEXED
     WHERE rowid BETWEEN ", b$lo, " AND ", b$hi, " AND YEAR = ", yr)))
  setkey(main, SAMPLE, SERIAL, PERNUM); setkey(wk, SAMPLE, SERIAL, PERNUM)
  d <- wk[main]

  d[, weeks      := weeks_worked(WKSWORK1, WKSWORK2)]
  d[, weeks_imp  := weeks_is_imputed(WKSWORK1, WKSWORK2)]
  d[, ann_hours  := fifelse(!is.na(weeks) & UHRSWORK %between% c(1, 99),
                            UHRSWORK * weeks, 0)]
  d[, lab_inc    := labor_income(INCWAGE, INCBUS, INCFARM, INCBUS00)]
  # y0 components: income that does not respond to hours
  d[, nonlab_inc := pmax(nas(INCINVST), 0, na.rm = TRUE) +
                    pmax(nas(INCOTHER), 0, na.rm = TRUE) +
                    pmax(nas(INCSS),    0, na.rm = TRUE) +
                    pmax(nas(INCWELFR), 0, na.rm = TRUE)]
  d[, YEAR := yr]
  d
}

message("Building model dataset for ", min(model_years), "-", max(model_years), " ...")
allp <- rbindlist(lapply(model_years, function(y) { message("  ", y); pull_year(y) }))

w <- allp[SEX == 2]; h <- allp[SEX == 1]

wf <- w[, .(YEAR, SERIAL, HHWT, STATEICP, COUNTYICP, HHINCOME, nchild = NCHILD,
            f_pn = PERNUM, f_sp = SPLOC, f_age = AGE, f_race = RACE, f_hisp = HISPAN,
            f_educ = EDUC, f_emp = EMPSTAT, f_h = ann_hours, f_lab = lab_inc,
            f_nonlab = nonlab_inc, f_wks_imp = weeks_imp)]
hs <- h[, .(YEAR, SERIAL, m_pn = PERNUM, m_sp = SPLOC, m_age = AGE, m_race = RACE,
            m_hisp = HISPAN, m_educ = EDUC, m_emp = EMPSTAT, m_h = ann_hours,
            m_lab = lab_inc, m_nonlab = nonlab_inc)]
pairs <- merge(wf, hs, by = c("YEAR", "SERIAL"), allow.cartesian = TRUE)
pairs <- pairs[f_sp == m_pn & m_sp == f_pn]          # mutual spouse link
message("  linked couples: ", format(nrow(pairs), big.mark = ","))

# ── Lump-sum exclusion ────────────────────────────────────────────────────
# Households whose reported labour income cannot have been earned at the hours
# reported -- a year's income in a week or two. Diagnosed on 1,034 flagged 2019
# cases: NOT self-employment (8.3% vs 6.8%), NOT imputation (8.2% allocated vs
# 16.6%); it is a denominator problem (median 77 annual hours; 32% report <= 4
# weeks). Top cases are payout years: $497k with 6 annual hours; $428k at
# OCC 10 (chief executives) with 2 hrs/wk.
#
# These are excluded as OUT OF SCOPE, not as outliers. Their income does not
# scale with hours at a constant wage, so the linear budget constraint
# c = w*h + y0 does not describe their choice set. Economically it is closer to
# a return on accumulated capital than a wage -- consistent with treating it as
# wealth rather than labour.
pairs[, drop_lump := (f_lab >= lump_income & f_h < min_ann_hours) |
                     (m_lab >= lump_income & m_h < min_ann_hours)]
message("  dropped as lump-sum/rentier: ", format(pairs[drop_lump == TRUE, .N], big.mark = ","),
        sprintf(" (%.3f%%)", 100 * mean(pairs$drop_lump)))
pairs <- pairs[drop_lump == FALSE]

# ── Wage: observed where hours are adequate, predicted otherwise ──────────
# w = labour income / annual hours is a genuine hourly price ONLY where hours
# are informative. Measured breakdown at low hours (2019, true weeks): wives at
# 1-14 hrs/wk show a mean implied wage of $81/hr vs $29 at full time, and 11%
# fall outside any plausible range. So below min_ann_hours the wage is treated
# as NOT MEASURED rather than measured badly -- the same status as a non-worker.
pairs[, f_w_obs := fifelse(f_h >= min_ann_hours, f_lab / f_h, NA_real_)]
pairs[, m_w_obs := fifelse(m_h >= min_ann_hours, m_lab / m_h, NA_real_)]

# Floor at half that year's federal minimum wage. A fixed dollar floor is
# meaningless across decades ($3.10 in 1980 vs $7.25 in 2019).
fed_min <- data.table(
  YEAR = 2010:2020,
  minw = c(7.25,7.25,7.25,7.25,7.25,7.25,7.25,7.25,7.25,7.25,7.25))
pairs <- merge(pairs, fed_min, by = "YEAR", all.x = TRUE)
pairs[, f_w_obs := fifelse(!is.na(f_w_obs) & f_w_obs < 0.5 * minw, NA_real_, f_w_obs)]
pairs[, m_w_obs := fifelse(!is.na(m_w_obs) & m_w_obs < 0.5 * minw, NA_real_, m_w_obs)]
# Top: within-year percentile trim (percentile, not a fixed dollar cap).
for (yy in unique(pairs$YEAR)) {
  q <- pairs[YEAR == yy & !is.na(f_w_obs), quantile(f_w_obs, 0.99, na.rm = TRUE)]
  pairs[YEAR == yy & !is.na(f_w_obs) & f_w_obs > q, f_w_obs := NA_real_]
  q <- pairs[YEAR == yy & !is.na(m_w_obs), quantile(m_w_obs, 0.99, na.rm = TRUE)]
  pairs[YEAR == yy & !is.na(m_w_obs) & m_w_obs > q, m_w_obs := NA_real_]
}

# Mincer wage equation, estimated on workers with adequate hours, used to
# PREDICT a wage for corner and marginal households. This is the standard
# approach in this literature (and BKP's own potential-earnings construction is
# the same idea). CAVEAT, stated: workers are positively selected, so predicted
# wages for non-workers are likely OVERSTATED, which biases the norm parameter
# UPWARD. Bounding that is preferable to a Heckman correction resting on an
# exclusion restriction ("children affect participation but not wages") that is
# itself contestable -- see Blundell et al. (2007) for the bounds approach.
educ5 <- function(e) fcase(is.na(e), NA_character_, e <= 5, "<HS", e == 6, "HS",
                           e %in% 7:9, "SC", e == 10, "C", e == 11, ">C",
                           default = NA_character_)
race3 <- function(r, hp) fcase(is.na(r) | is.na(hp), NA_character_,
                               hp %in% 1:4, "Hisp", hp == 0 & r == 1, "White",
                               hp == 0 & r == 2, "Black", default = "Other")
pairs[, `:=`(f_e5 = educ5(f_educ), m_e5 = educ5(m_educ),
             f_r3 = race3(f_race, f_hisp), m_r3 = race3(m_race, m_hisp))]

fit_wage <- function(dt, wcol, acol, ecol, rcol) {
  d <- dt[!is.na(get(wcol)) & get(wcol) > 0]
  lm(log(get(wcol)) ~ poly(get(acol), 2) + factor(get(ecol)) + factor(get(rcol)) +
       factor(STATEICP) + factor(YEAR), data = d, weights = d$HHWT)
}
mw_f <- fit_wage(pairs, "f_w_obs", "f_age", "f_e5", "f_r3")
mw_m <- fit_wage(pairs, "m_w_obs", "m_age", "m_e5", "m_r3")
message("  Mincer wage eq (wife):    n = ", format(nobs(mw_f), big.mark = ","),
        "  R2 = ", round(summary(mw_f)$r.squared, 3))
message("  Mincer wage eq (husband): n = ", format(nobs(mw_m), big.mark = ","),
        "  R2 = ", round(summary(mw_m)$r.squared, 3))

pred_ok_f <- !is.na(pairs$f_e5) & !is.na(pairs$f_r3)
pred_ok_m <- !is.na(pairs$m_e5) & !is.na(pairs$m_r3)
pairs[, f_w_hat := NA_real_]; pairs[, m_w_hat := NA_real_]
pairs[pred_ok_f, f_w_hat := exp(predict(mw_f, newdata = .SD)), .SDcols = names(pairs)]
pairs[pred_ok_m, m_w_hat := exp(predict(mw_m, newdata = .SD)), .SDcols = names(pairs)]

pairs[, f_w := fifelse(!is.na(f_w_obs), f_w_obs, f_w_hat)]
pairs[, m_w := fifelse(!is.na(m_w_obs), m_w_obs, m_w_hat)]
pairs[, f_w_predicted := is.na(f_w_obs)]
pairs[, m_w_predicted := is.na(m_w_obs)]

# ── y0, z_W, and the model's other inputs ────────────────────────────────
pairs[, y0   := f_nonlab + m_nonlab]      # actual non-labour income, not a residual
pairs[, y    := HHINCOME]                 # affordability scale (see note below)
pairs[, z_W  := fifelse((f_lab + m_lab) > 0, f_lab / (f_lab + m_lab), NA_real_)]
pairs[, regime := fcase(f_h == 0, "corner (h_f = 0)",
                        f_h < min_ann_hours, "marginal hours",
                        default = "interior")]

out <- pairs[, .(YEAR, SERIAL, HHWT, STATEICP, COUNTYICP, nchild,
                 f_age, m_age, f_e5, m_e5, f_r3, m_r3, f_emp, m_emp,
                 f_h, m_h, f_lab, m_lab, f_w, m_w, f_w_obs, m_w_obs,
                 f_w_predicted, m_w_predicted, f_wks_imp,
                 y0, y, z_W, regime)]
f <- file.path(panel_dir, "model_input_households.csv")
fwrite(out, f)

message("
=== model dataset ===")
message("  households: ", format(nrow(out), big.mark = ","))
print(out[, .(couples = .N, pct = round(100 * .N / nrow(out), 1),
              mean_wife_hours = round(mean(f_h)),
              mean_wife_wage  = round(mean(f_w, na.rm = TRUE), 2),
              wage_predicted_pct = round(100 * mean(f_w_predicted), 1)), by = regime][order(-couples)])
message("
  z_W > 0.5 (wife out-earns): ",
        sprintf("%.1f%%", 100 * mean(out$z_W > 0.5, na.rm = TRUE)))
message("  mean y0 (non-labour income): $", format(round(mean(out$y0)), big.mark = ","))
message("  weeks imputed (2008-2018 interval reconstruction): ",
        sprintf("%.1f%%", 100 * mean(out$f_wks_imp, na.rm = TRUE)))
message("
written: ", f)
