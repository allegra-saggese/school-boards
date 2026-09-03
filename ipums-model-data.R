library(data.table)
set.seed(20260830)   # reproducible residual draws in the wage imputation
library(DBI)
library(RSQLite)

source(here::here("_setup.R"))

# Builds the household-level inputs for the T3 model.
# Input  : data/interim/ipums_bkp.sqlite
# Output : data/processed/panel/model_input_households.csv

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
#        A wage built from (INCTOT - INCSS - INCWELFR)/hours would fail (a):
#        it carries capital income and inflates the implied wage for
#        asset-holders.
#
#  y0    Income entering the budget that does NOT respond to hours: capital,
#        retirement, and transfer income.
#        Built directly, not as the residual hhincome - both spouses' income:
#        that double-counts, since capital income sits inside w*h AND is
#        subtracted from y0.
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

# ALL years the model's inputs exist for. 1970 is excluded because INCINVST is
# absent that year, so y0 (non-labour income) cannot be constructed; 1980
# onward has full coverage. The former 2010:2020 window was an arbitrary
# calibration choice, not a data constraint -- T3 uses no county or political
# data, so it is not bound by T2's 2012-2020 window.
model_years    <- c(1980L, 1990L, 2000L, 2001:2024)
# Override for testing, e.g. MODEL_YEARS=1980,2019
if (nzchar(Sys.getenv("MODEL_YEARS")))
  model_years <- as.integer(strsplit(Sys.getenv("MODEL_YEARS"), ",")[[1]])
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

# ── STREAM year by year ──────────────────────────────────────────────────────
# Holding 28 years in memory at once exceeds R's 24 GB vector limit (this OOMed
# on the first attempt). Every downstream step is year-separable -- the wage
# trims were already per-year, and the Mincer equation is now fitted per year
# too (see below) -- so each year is processed end to end and appended to disk.
message("Building model dataset for ", min(model_years), "-", max(model_years), " ...")

out_file <- file.path(panel_dir, "model_input_households.csv")
if (file.exists(out_file)) file.remove(out_file)
first_year <- TRUE

for (.yr in model_years) {
message("  ", .yr)
allp <- pull_year(.yr)

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
# Full federal minimum wage history. Indexed on the INCOME REFERENCE year
# (YEAR - 1), since ACS and census income questions cover the previous 12
# months. Statutory changes: $1.60 (1968), $2.00 (May 1974), $2.10 (1975),
# $2.30 (1976), $2.65 (1978), $2.90 (1979), $3.10 (1980), $3.35 (1981),
# $3.80 (Apr 1990), $4.25 (Apr 1991), $4.75 (Oct 1996), $5.15 (Sep 1997),
# $5.85 (Jul 2007), $6.55 (Jul 2008), $7.25 (Jul 2009, unchanged since).
minw_by_income_year <- data.table(
  iyear = 1969:2023,
  minw  = c(rep(1.60, 5),                       # 1969-1973
            2.00, 2.10, 2.30, 2.30, 2.65, 2.90, # 1974-1979
            3.10, rep(3.35, 9),                 # 1980, 1981-1989
            3.80, rep(4.25, 5),                 # 1990, 1991-1995
            4.75, rep(5.15, 10),                # 1996, 1997-2006
            5.85, 6.55, rep(7.25, 15)))         # 2007, 2008, 2009-2023
stopifnot(nrow(minw_by_income_year) == length(1969:2023))
fed_min <- minw_by_income_year[, .(YEAR = iyear + 1L, minw)]
pairs <- merge(pairs, fed_min, by = "YEAR", all.x = TRUE)
if (any(is.na(pairs$minw))) stop("federal minimum wage missing for year(s): ",
    paste(sort(unique(pairs[is.na(minw)]$YEAR)), collapse = ", "))
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

# Fitted SEPARATELY EACH YEAR. The previous version pooled all years with only
# year fixed effects, holding the returns to education and experience constant
# across the whole sample. Over 1980-2024 that is untenable -- the college wage
# premium roughly doubled over this period -- and it would push that
# mis-specification straight into the imputed wages for non-workers, who are
# exactly the households at the corner. factor(YEAR) is dropped because it is
# collinear within a single year.
fit_wage <- function(dt, wcol, acol, ecol, rcol) {
  d <- dt[!is.na(get(wcol)) & get(wcol) > 0]
  lm(log(get(wcol)) ~ poly(get(acol), 2) + factor(get(ecol)) + factor(get(rcol)) +
       factor(STATEICP), data = d, weights = d$HHWT)
}
mw_f <- fit_wage(pairs, "f_w_obs", "f_age", "f_e5", "f_r3")
mw_m <- fit_wage(pairs, "m_w_obs", "m_age", "m_e5", "m_r3")
message("  Mincer wage eq (wife):    n = ", format(nobs(mw_f), big.mark = ","),
        "  R2 = ", round(summary(mw_f)$r.squared, 3))
message("  Mincer wage eq (husband): n = ", format(nobs(mw_m), big.mark = ","),
        "  R2 = ", round(summary(mw_m)$r.squared, 3))

# IMPUTE FROM THE PREDICTIVE DISTRIBUTION, NOT THE POINT PREDICTION.
# exp(X'b) alone assigns every non-worker a near-median wage. Measured on 2003,
# that halved the dispersion of imputed wages relative to observed ones
# (SD of log 0.301 vs 0.601) and raised the BOTTOM of the distribution by 40%
# (p10 $9.06 imputed vs $6.49 observed). With the Mincer R2 only ~0.23, three
# quarters of the variance was being discarded.
#
# That compression is not cosmetic. It erases both tails of the non-worker wage
# distribution: the low-wage women who do not work because work does not pay,
# and the high-wage women who do not work despite it paying. The model then
# sees non-workers with near-median wages, concludes they should be working,
# and under-predicts non-participation at the bottom of the distribution.
#
# Adding a residual draw restores the dispersion the regression threw away and
# also removes the retransformation bias -- exp(X'b) is not an unbiased
# estimate of E[w|X] under log-linearity in any case.
#
# Residuals are BOOTSTRAPPED from the fitted residuals rather than drawn
# normal, so any skewness in the wage distribution is preserved.
#
# NOTE what this does NOT fix: selection. The residuals come from a
# workers-only regression, so if workers are positively selected the whole
# imputed distribution is still shifted up. This restores the VARIANCE, not the
# MEAN. A Heckman correction would be the next step -- with children as the
# exclusion restriction, never the husband's earnings, since the model claims
# his earnings act on her participation through the norm.
draw_resid <- function(model, k) {
  r <- residuals(model)
  r[sample.int(length(r), k, replace = TRUE)]
}
pred_ok_f <- !is.na(pairs$f_e5) & !is.na(pairs$f_r3)
pred_ok_m <- !is.na(pairs$m_e5) & !is.na(pairs$m_r3)
pairs[, f_w_hat := NA_real_]; pairs[, m_w_hat := NA_real_]
pairs[pred_ok_f, f_w_hat := exp(predict(mw_f, newdata = .SD) +
                                draw_resid(mw_f, sum(pred_ok_f))), .SDcols = names(pairs)]
pairs[pred_ok_m, m_w_hat := exp(predict(mw_m, newdata = .SD) +
                                draw_resid(mw_m, sum(pred_ok_m))), .SDcols = names(pairs)]
# Guard against absurd draws in the extreme tail of the residual distribution.
for (cc in c("f_w_hat", "m_w_hat")) {
  hi <- pairs[, quantile(get(cc), 0.999, na.rm = TRUE)]
  pairs[get(cc) > hi, (cc) := hi]
}

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
fwrite(out, out_file, append = !first_year)
first_year <- FALSE
rm(allp, w, h, wf, hs, pairs, out); invisible(gc())
}   # end per-year loop

f <- out_file
out <- fread(out_file, showProgress = FALSE)   # reload for the summary below

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
