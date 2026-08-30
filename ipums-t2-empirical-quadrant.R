# =============================================================================
# T2 — EMPIRICAL: the culture x wealth quadrant
#
# PART 2 of three. T1 is the BKP replication (ipums-bkp-pure-replication.R);
# T3 is the theoretical household utility model. This script is T2 and is
# EXPLORATORY by design: it does not identify a causal effect, it asks whether
# a norm is visible in the data at all, and if so, whether that norm is elastic
# to WEALTH or to CULTURE.
#
# DESIGN — sort married couples into a 2x2 and compare the wife's labour supply
# across the four cells:
#
#                        not wealthy          wealthy
#     conservative   |  cons / poor      |  cons / wealthy  |
#     progressive    |  prog / poor      |  prog / wealthy  |
#
# on two margins, because a norm can bind on either and they need not agree:
#     (1) EXTENSIVE — does she participate at all (LFP)
#     (2) INTENSIVE — given the choice set, how many hours
#
# PROXIES (both are proxies; neither is the thing itself — see LIMITATIONS)
#   culture : county presidential vote margin, Republican- vs Democratic-majority
#   wealth  : the couple's ASSET income (INCINVST — interest, dividends, rent)
#
# WHY ASSET INCOME FOR WEALTH, AND THE ONE THING THIS SCRIPT MUST NOT DO
#   Asset income is a flow thrown off by a stock, so it proxies assets while
#   containing no labour earnings by construction. That matters because the
#   whole point of the wealth axis is to be something OTHER than the husband's
#   pay-cheque. If "wealthy" were defined on total or labour income, the
#   quadrant would silently re-measure the income gradient T1 already
#   estimated, and any "wealth elasticity" we reported would be circular.
#   Two guards against that:
#     (a) the wealth axis excludes labour earnings by construction, and
#     (b) EVERY quadrant statistic is ALSO computed WITHIN labour-income
#         decile (Section 6), which holds earnings roughly fixed and leaves
#         only the asset variation. If the quadrant gaps survive (b), they are
#         about wealth. If they collapse, they were about income all along.
#         This comparison is the main result of the script.
#
# LIMITATIONS — stated here because they bound what T2 can support
#   * Asset income is positive for only ~15% of couples in this window and its
#     reported incidence FALLS over time (23.9% of adults in 1980 -> 8-9% by
#     2022), almost certainly reporting decay plus the migration of assets into
#     tax-deferred accounts that pay no received dividend. So "wealthy" here
#     means "reports asset income," which is a noisy, downward-biased
#     indicator of holding assets. Housing wealth (VALUEH/OWNERSHP), the
#     median household's dominant asset, is NOT in extract #4 and would be the
#     better axis; it needs a supplementary extract.
#   * County vote margin is a CONTEXTUAL measure. It is the politics of the
#     place, not of the couple, so cell assignment is measured with error and
#     any culture gap is attenuated toward zero. It also cannot separate
#     ideology from everything else that varies across counties (industry mix,
#     urbanisation, cost of living); Section 5's state fixed effects absorb
#     the coarsest version of that, nothing absorbs the rest.
#   * County is identified for only ~61% of ACS households, and the missing
#     share is systematically small and rural.
#   * Repeated cross-sections. No couple is observed twice; nothing here is a
#     within-couple change.
# =============================================================================

library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)
library(fixest)

source("functions.R")
source("R/paths.R")

# ── 0) Config ────────────────────────────────────────────────────────────────
# Window is set by the political data, which covers 2012-2020 only.
t2_years    <- 2012:2020
age_lo      <- 18L
age_hi      <- 65L

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)
sqlite_path <- data_path("interim", "ipums_bkp.sqlite")

# ── 1) Helpers ───────────────────────────────────────────────────────────────
# IPUMS codes "not applicable"/"missing" as large sentinels rather than NA.
# Left in place they would be summed as if they were dollars.
nas <- function(x, cut = 999990) {
  x <- as.numeric(x)
  fifelse(is.na(x) | x >= cut, NA_real_, x)
}

# BKP's labour-income concept: wages plus self-employment. INCBUS00 supersedes
# INCBUS/INCFARM from 2000 on; before that the two components are summed.
labor_income <- function(incwage, incbus, incfarm, incbus00) {
  w  <- nas(incwage); b0 <- nas(incbus00)
  b  <- nas(incbus);  f  <- nas(incfarm)
  self_emp <- fifelse(!is.na(b0), b0, rowSums(cbind(b, f), na.rm = TRUE))
  pmax(rowSums(cbind(w, self_emp), na.rm = TRUE), 0)
}

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# ── 2) Pull the couples ──────────────────────────────────────────────────────
# NOT INDEXED is deliberate. The only index on this table is (AGE, SEX); with
# an AGE predicate SQLite will otherwise pick it and turn a scan into ~10^8
# index seeks, which measured 290x slower on this database.
cache_path <- Sys.getenv("T2_CACHE_WRITE", file.path(tempdir(), "t2_quad_sample.rds"))
cache_env  <- Sys.getenv("T2_CACHE", "")
use_cache  <- nzchar(cache_env) && file.exists(cache_env)

if (use_cache) {
  message("Reusing cached T2 sample: ", cache_env)
  quad <- readRDS(cache_env)
} else {
message("Pulling ", min(t2_years), "-", max(t2_years), " spouse-linked adults ...")

con <- dbConnect(SQLite(), sqlite_path)
on.exit(dbDisconnect(con), add = TRUE)

persons <- setDT(dbGetQuery(con, sprintf("
  SELECT YEAR, SAMPLE, SERIAL, PERNUM, SPLOC, SEX, AGE, STATEICP, COUNTYICP,
         HHWT, EDUC, RACE, HISPAN, NCHILD, LABFORCE, EMPSTAT,
         UHRSWORK, WKSWORK1,
         INCWAGE, INCBUS, INCBUS00, INCFARM, INCTOT,
         INCINVST, INCRETIR, INCOTHER
  FROM ipums_table NOT INDEXED
  WHERE YEAR BETWEEN %d AND %d
    AND AGE BETWEEN %d AND %d
    AND SPLOC > 0
    AND COUNTYICP > 0",
  min(t2_years), max(t2_years), age_lo, age_hi)))

message("  spouse-linked adults 18-65 with county: ", format(nrow(persons), big.mark = ","))

# ── 3) Build opposite-sex couples on a MUTUAL spouse link ────────────────────
# SPLOC holds the PERNUM of the spouse. Requiring the link to point back
# (m.PERNUM == f.SPLOC AND m.SPLOC == f.PERNUM) drops the one-directional
# links that arise in complex households.
wives    <- persons[SEX == 2L]
husbands <- persons[SEX == 1L]

setnames(wives,    setdiff(names(wives),    c("YEAR","SAMPLE","SERIAL")),
         paste0("f_", setdiff(names(wives),    c("YEAR","SAMPLE","SERIAL"))))
setnames(husbands, setdiff(names(husbands), c("YEAR","SAMPLE","SERIAL")),
         paste0("m_", setdiff(names(husbands), c("YEAR","SAMPLE","SERIAL"))))

pairs <- merge(wives, husbands,
               by.x = c("YEAR","SAMPLE","SERIAL","f_SPLOC"),
               by.y = c("YEAR","SAMPLE","SERIAL","m_PERNUM"),
               all = FALSE)
setnames(pairs, "f_SPLOC", "m_PERNUM")
pairs <- pairs[m_SPLOC == f_PERNUM]           # mutual link only
rm(persons, wives, husbands); invisible(gc())

message("  mutually-linked opposite-sex couples: ", format(nrow(pairs), big.mark = ","))

# ── 4) Variables ─────────────────────────────────────────────────────────────
# 4a. Labour supply — the two outcome margins.
# LABFORCE: 1 = not in labour force, 2 = in labour force.
pairs[, f_lfp := fifelse(f_LABFORCE %in% c(1L, 2L), as.numeric(f_LABFORCE == 2L), NA_real_)]
# UHRSWORK/WKSWORK1 use 0 as "not applicable", which for a non-worker is a
# true zero of hours. Zeroes are kept in the unconditional hours measure and
# excluded from the conditional one.
pairs[, f_weekly_hours := fifelse(is.na(f_UHRSWORK), NA_real_, as.numeric(f_UHRSWORK))]
pairs[, f_annual_hours := f_weekly_hours * fifelse(is.na(f_WKSWORK1), NA_real_, as.numeric(f_WKSWORK1))]
pairs[, f_hours_if_working := fifelse(f_weekly_hours > 0, f_weekly_hours, NA_real_)]

# 4b. Earnings.
pairs[, f_labinc := labor_income(f_INCWAGE, f_INCBUS, f_INCFARM, f_INCBUS00)]
pairs[, m_labinc := labor_income(m_INCWAGE, m_INCBUS, m_INCFARM, m_INCBUS00)]
pairs[, couple_labinc := f_labinc + m_labinc]

# 4c. WEALTH AXIS — couple asset income. Contains no labour earnings.
pairs[, f_assetinc := pmax(nas(f_INCINVST), 0)]
pairs[, m_assetinc := pmax(nas(m_INCINVST), 0)]
pairs[, couple_assetinc := rowSums(cbind(f_assetinc, m_assetinc), na.rm = TRUE)]
pairs[is.na(f_assetinc) & is.na(m_assetinc), couple_assetinc := NA_real_]

# Binary split. Asset income is ~85% zero, so quantile cuts are undefined in
# the lower ties -- "reports any asset income" is the only split the data
# actually supports. A stricter tier is carried alongside it as a robustness
# check on the same axis.
pairs[, wealthy := fifelse(is.na(couple_assetinc), NA_character_,
                           fifelse(couple_assetinc > 0, "Wealthy", "Not wealthy"))]
# Strict tier: top decile OF ASSET HOLDERS, cut within year so the threshold
# tracks the (declining) reporting rate rather than a fixed dollar amount.
p90 <- pairs[couple_assetinc > 0,
             .(asset_p90 = quantile(couple_assetinc, 0.90, na.rm = TRUE)), by = YEAR]
pairs <- merge(pairs, p90, by = "YEAR", all.x = TRUE)
pairs[, wealthy_strict := fifelse(
  is.na(couple_assetinc) | is.na(asset_p90), NA_character_,
  fifelse(couple_assetinc >= asset_p90, "Top-decile asset holders", "Other"))]
pairs[, ln_assetinc := log1p(pmax(couple_assetinc, 0))]

# 4d. CULTURE AXIS — county vote margin.
pairs[, fips := build_county_fips(f_STATEICP, f_COUNTYICP)]

pol_files <- list.files(panel_dir, pattern = "_lfpr_panel_with_groups[.]csv$",
                        full.names = TRUE)
pol <- fread(sort(pol_files)[length(pol_files)],
             select = c("fips", "year", "vote_margin"))
# fread types FIPS as integer and strips leading zeros; both sides must be
# padded or every state numbered 01-09 silently fails to merge.
pol[, fips := pad_fips(fips)]
setnames(pol, "year", "YEAR")
pol <- unique(pol[!is.na(vote_margin) & !is.na(fips)], by = c("fips", "YEAR"))

pairs <- merge(pairs, pol, by = c("fips", "YEAR"), all.x = TRUE)
message("  couples matched to a county vote margin: ",
        format(sum(!is.na(pairs$vote_margin)), big.mark = ","),
        " (", round(100 * mean(!is.na(pairs$vote_margin)), 1), "%)")

pairs[, culture := fifelse(is.na(vote_margin), NA_character_,
                           fifelse(vote_margin < 0, "Conservative", "Progressive"))]

# 4e. Frontier (Bazzi et al.) — a second, independent culture proxy.
frontier <- fread(file.path(panel_dir, "bazzi_frontier_indicators.csv"),
                  select = c("fips", "is_frontier"))
frontier[, fips := pad_fips(fips)]
frontier <- unique(frontier, by = "fips")
pairs <- merge(pairs, frontier, by = "fips", all.x = TRUE)

# 4f. Controls.
pairs[, `:=`(
  year_f    = factor(YEAR),
  state_f   = factor(f_STATEICP),
  f_age     = as.numeric(f_AGE),
  m_age     = as.numeric(m_AGE),
  f_college = as.numeric(f_EDUC >= 10L),
  m_college = as.numeric(m_EDUC >= 10L),
  any_kids  = as.numeric(f_NCHILD > 0L),
  ln_m_labinc = log1p(pmax(m_labinc, 0))
)]

# Labour-income decile, computed WITHIN year so it is a rank not a dollar
# threshold, and on COUPLE labour income so it is symmetric in the spouses.

quad <- pairs[!is.na(culture) & !is.na(wealthy)]
quad[, quadrant := paste0(culture, " / ", wealthy)]
message("  analysis sample (culture + wealth both defined): ",
        format(nrow(quad), big.mark = ","))

rm(pairs); invisible(gc())
saveRDS(quad, cache_path)
message("  cached sample for re-runs: ", cache_path,
        "  (re-run with T2_CACHE=", cache_path, " to skip the scan)")
}

quad[, conservative := as.numeric(culture == "Conservative")]
quad[, wealth_bin   := as.numeric(wealthy == "Wealthy")]

# ── Income rank: condition on the HUSBAND'S earnings, not the couple's ───────
# This is deliberate and it matters. couple_labinc = his + HERS, so ranking on
# it would condition on the dependent variable: a wife who does not work drags
# her household into a lower decile by construction. Comparing wealthy and
# non-wealthy couples "within decile" would then be partly a comparison of
# households selected on her labour supply -- a textbook bad control, and it
# would inflate the wealth effect rather than purge it.
# The husband's labour income is the right conditioning set: it captures the
# couple's earnings capacity without containing her participation decision.
# (Not perfectly exogenous -- he may adjust hours in response to her -- but it
# does not contain the outcome mechanically.)
# Rank-based rather than cut(quantile(...)): with mass points in the earnings
# distribution the quantile breaks are not guaranteed unique, and cut() errors
# on duplicate breaks instead of degrading gracefully.
quad[m_labinc > 0,
     husb_decile := as.integer(pmin(10, floor(10 * (frank(m_labinc, ties.method = "average") - 0.5) / .N) + 1)),
     by = YEAR]
# Couple-income version retained ONLY as the contaminated comparison, so the
# size of the bad-control bias is visible rather than hidden.
quad[couple_labinc > 0,
     labinc_decile := as.integer(pmin(10, floor(10 * (frank(couple_labinc, ties.method = "average") - 0.5) / .N) + 1)),
     by = YEAR]

quad[, ageband := cut(f_age, c(17, 29, 39, 49, 59, 65),
                      labels = c("18-29", "30-39", "40-49", "50-59", "60-65"))]

# ── 5) The quadrant ──────────────────────────────────────────────────────────
message("\n=== T2.1  THE QUADRANT: wife's labour supply by culture x wealth ===")

quad_tab <- quad[, .(
  couples          = .N,
  wtd_couples      = sum(f_HHWT, na.rm = TRUE),
  lfp_pct          = 100 * wmean(f_lfp, f_HHWT),
  weekly_hours     = wmean(f_weekly_hours, f_HHWT),
  hours_if_working = wmean(f_hours_if_working, f_HHWT),
  couple_labinc    = wmean(couple_labinc, f_HHWT),
  asset_income     = wmean(couple_assetinc, f_HHWT)
), by = .(culture, wealthy)][order(culture, wealthy)]

print(quad_tab)

# The two elasticities the quadrant exists to separate. Each is a difference
# of differences within the 2x2: the wealth gap holds culture fixed and the
# culture gap holds wealth fixed.
gap <- function(dt, outcome) {
  g <- function(cul, wl) dt[culture == cul & wealthy == wl][[outcome]]
  c(wealth_gap_within_conservative = g("Conservative", "Wealthy") - g("Conservative", "Not wealthy"),
    wealth_gap_within_progressive  = g("Progressive",  "Wealthy") - g("Progressive",  "Not wealthy"),
    culture_gap_within_notwealthy  = g("Conservative", "Not wealthy") - g("Progressive", "Not wealthy"),
    culture_gap_within_wealthy     = g("Conservative", "Wealthy")     - g("Progressive", "Wealthy"))
}
message("\nLFP gaps (percentage points):")
print(round(gap(quad_tab, "lfp_pct"), 3))
message("\nWeekly-hours gaps:")
print(round(gap(quad_tab, "weekly_hours"), 3))

fwrite(quad_tab, dated_path(results_dir, "t2_quadrant_means.csv"))

# ── 6) THE MAIN RESULT: the same quadrant WITHIN labour-income decile ────────
# If the wealth gaps in Section 5 are really an income gradient wearing a
# wealth costume, they collapse once earnings are held roughly fixed here.
message("\n=== T2.2  QUADRANT WITHIN LABOUR-INCOME DECILE ===")

quad_dec <- quad[!is.na(labinc_decile), .(
  couples      = .N,
  lfp_pct      = 100 * wmean(f_lfp, f_HHWT),
  weekly_hours = wmean(f_weekly_hours, f_HHWT)
), by = .(labinc_decile, culture, wealthy)][order(labinc_decile, culture, wealthy)]

dec_gaps <- rbindlist(lapply(sort(unique(quad_dec$labinc_decile)), function(d) {
  sub <- quad_dec[labinc_decile == d]
  if (nrow(sub) < 4L) return(NULL)
  data.table(labinc_decile = d,
             t(gap(sub, "lfp_pct")),
             weekly_hours_wealth_gap_cons = gap(sub, "weekly_hours")[1],
             weekly_hours_culture_gap_nw  = gap(sub, "weekly_hours")[3])
}))
message("\nLFP gaps (pp) by labour-income decile — do they survive conditioning on earnings?")
print(dec_gaps)

fwrite(quad_dec, dated_path(results_dir, "t2_quadrant_by_income_decile.csv"))
fwrite(dec_gaps, dated_path(results_dir, "t2_quadrant_gaps_by_income_decile.csv"))

# ── 7) Regressions — state FE throughout ────────────────────────────────────
# Section 5 is unconditional; these hold demographics fixed and absorb state
# and year. State FE matter here because both axes are geographic: without
# them a "culture" gap could be Alabama-vs-Massachusetts in anything at all.
# SEs clustered on county, the level at which the culture proxy varies.
message("\n=== T2.3  REGRESSIONS (state + year FE, SE clustered on county) ===")

ctrl <- "f_age + I(f_age^2) + m_age + f_college + m_college + any_kids + ln_m_labinc"

m_lfp <- feols(as.formula(paste0(
  "f_lfp ~ conservative * wealth_bin + ", ctrl, " | state_f + year_f")),
  data = quad, weights = ~f_HHWT, cluster = ~fips)

m_hrs <- feols(as.formula(paste0(
  "f_weekly_hours ~ conservative * wealth_bin + ", ctrl, " | state_f + year_f")),
  data = quad, weights = ~f_HHWT, cluster = ~fips)

# Continuous versions: the binary wealth split throws away all variation among
# asset holders, and the binary culture split throws away margin intensity.
m_lfp_cont <- feols(as.formula(paste0(
  "f_lfp ~ vote_margin * ln_assetinc + ", ctrl, " | state_f + year_f")),
  data = quad, weights = ~f_HHWT, cluster = ~fips)

etable(m_lfp, m_hrs, m_lfp_cont,
       headers = c("LFP (binary axes)", "Weekly hours", "LFP (continuous axes)"),
       digits = 4)

t2_coefs <- rbindlist(lapply(
  list(`LFP (binary)` = m_lfp, `Weekly hours` = m_hrs, `LFP (continuous)` = m_lfp_cont),
  function(m) {
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    setnames(ct, c("term", "estimate", "std_error", "t_value", "p_value"))
    ct[term %like% "conservative|wealth_bin|vote_margin|ln_assetinc"]
  }), idcol = "model")
fwrite(t2_coefs, dated_path(results_dir, "t2_regression_coefficients.csv"))

# ── 8) Plots ─────────────────────────────────────────────────────────────────
save_plot("t2_quadrant_lfp_and_hours.png", {
  pd <- melt(quad_tab, id.vars = c("culture", "wealthy"),
             measure.vars = c("lfp_pct", "weekly_hours"),
             variable.name = "outcome", value.name = "value")
  pd[, outcome := factor(outcome, levels = c("lfp_pct", "weekly_hours"),
        labels = c("Wife's labour force participation (%)",
                   "Wife's usual weekly hours (incl. zeros)"))]
  print(
    ggplot(pd, aes(x = wealthy, y = value, fill = culture)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = sprintf("%.1f", value)),
                position = position_dodge(width = 0.8), vjust = -0.4, size = 3.2) +
      facet_wrap(~outcome, scales = "free_y") +
      scale_fill_manual(values = c(Conservative = "#B2182B", Progressive = "#2166AC")) +
      labs(title = "T2: wife's labour supply by culture x wealth quadrant",
           subtitle = paste0("Married couples, both 18-65, ", min(t2_years), "-", max(t2_years),
                             ". Culture = county vote margin; wealth = couple reports asset income.",
                             "\nUnconditional means, household-weighted."),
           x = NULL, y = NULL, fill = "County lean",
           caption = "Source: IPUMS USA. Exploratory; see script header for proxy limitations.") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  )
}, width = 2200, height = 1200)

save_plot("t2_quadrant_lfp_by_income_decile.png", {
  pd <- quad_dec[!is.na(labinc_decile)]
  pd[, quadrant := paste0(culture, " / ", wealthy)]
  print(
    ggplot(pd, aes(x = labinc_decile, y = lfp_pct, colour = culture, linetype = wealthy)) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
      scale_x_continuous(breaks = 1:10) +
      scale_colour_manual(values = c(Conservative = "#B2182B", Progressive = "#2166AC")) +
      labs(title = "T2: does the quadrant survive conditioning on earnings?",
           subtitle = paste0("Wife's LFP by couple labour-income decile (within year), ",
                             min(t2_years), "-", max(t2_years),
                             ".\nIf wealth gaps were an income gradient in disguise, the ",
                             "solid/dashed pairs would converge."),
           x = "Couple labour-income decile (within year)",
           y = "Wife's labour force participation (%)",
           colour = "County lean", linetype = "Asset income") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  )
}, width = 2200, height = 1300)

# ── 9) IS THE NORM INCOME-ELASTIC? The quadrant re-estimated DECILE BY DECILE ─
# Section 6 showed the raw gaps by decile; this repeats it with the full control
# set and fixed effects, so the decile profile cannot be an age or education
# composition effect. This is the direct test of the project's central claim:
# an income-ELASTIC norm means the culture x wealth interaction should be
# absent at the bottom of the earnings distribution and present at the top.
# A norm that is a fixed cultural constant would instead be flat across deciles.
message("\n=== T2.4  DECILE-BY-DECILE: is the norm income-elastic? ===")

dec_list <- sort(unique(quad[!is.na(husb_decile)]$husb_decile))
dec_coefs <- rbindlist(lapply(dec_list, function(d) {
  sub <- quad[husb_decile == d]
  if (nrow(sub) < 5000L || uniqueN(sub$state_f) < 2L) return(NULL)
  m <- tryCatch(feols(as.formula(paste0(
         "f_lfp ~ conservative * wealth_bin + ", ctrl, " | state_f + year_f")),
         data = sub, weights = ~f_HHWT, cluster = ~fips),
       error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "estimate", "std_error", "t_value", "p_value"))
  ct <- ct[term %in% c("conservative", "wealth_bin", "conservative:wealth_bin")]
  ct[, `:=`(husb_decile = d, n = nrow(sub))]
  ct
}))
dec_coefs[, `:=`(ci_lo = estimate - 1.96 * std_error,
                 ci_hi = estimate + 1.96 * std_error)]

message("\nLFP effects by couple labour-income decile (pp, controls + state/year FE):")
print(dec_coefs[, .(husb_decile, term,
                    est_pp = round(100 * estimate, 2),
                    lo_pp  = round(100 * ci_lo, 2),
                    hi_pp  = round(100 * ci_hi, 2),
                    p = signif(p_value, 2))][order(term, labinc_decile)])

fwrite(dec_coefs, dated_path(results_dir, "t2_decile_coefficients.csv"))

save_plot("t2_income_elasticity_of_the_norm.png", {
  pd <- copy(dec_coefs)
  pd[, term := factor(term,
       levels = c("wealth_bin", "conservative", "conservative:wealth_bin"),
       labels = c("Wealth effect (asset income)",
                  "Culture effect (conservative county)",
                  "Culture x wealth interaction"))]
  print(
    ggplot(pd, aes(x = husb_decile, y = 100 * estimate)) +
      geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
      geom_ribbon(aes(ymin = 100 * ci_lo, ymax = 100 * ci_hi),
                  fill = "#4292C6", alpha = 0.25) +
      geom_line(colour = "#08519C", linewidth = 0.9) +
      geom_point(colour = "#08519C", size = 1.8) +
      facet_wrap(~term, nrow = 1, scales = "free_y") +
      scale_x_continuous(breaks = 1:10) +
      labs(title = "T2: is the breadwinner norm income-elastic?",
           subtitle = paste0("Effect on wife's LFP, estimated separately within each couple ",
                             "husband's labour-income decile, ", min(t2_years), "-", max(t2_years),
                             ".\nControls: both ages, both education, children, husband's ",
                             "labour income; state and year fixed effects. 95% CI, SE clustered on county."),
           x = "Husband's labour-income decile (within year)",
           y = "Effect on wife's LFP (percentage points)",
           caption = "Source: IPUMS USA + county presidential returns. Exploratory; proxies described in script header.") +
      theme_minimal(base_size = 12) +
      theme(panel.spacing = unit(1.2, "lines"))
  )
}, width = 2600, height = 1100)

# ── 10) ROBUSTNESS: does the pooled interaction survive FLEXIBLE income control? ─
# Section 7's pooled model controls for income only as linear ln(husband's
# labour income). Section 9 estimates the same interaction separately within
# each decile and finds it ~0 in deciles 1-9. Those two cannot both be right:
# if the interaction were genuinely -2pp everywhere, the decile-wise estimates
# would show it. The suspicion is that the pooled interaction is absorbing
# cross-decile COMPOSITION rather than a within-income cultural effect.
# This section adjudicates by adding income-decile fixed effects, and then by
# letting the whole control set vary by decile.
message("
=== T2.5  Is the pooled interaction robust to flexible income controls? ===")

qd <- quad[!is.na(husb_decile)]
qd[, decile_f     := factor(husb_decile)]
qd[, bad_decile_f := factor(labinc_decile)]

r1 <- feols(as.formula(paste0("f_lfp ~ conservative * wealth_bin + ", ctrl,
                              " | state_f + year_f")),
            data = qd, weights = ~f_HHWT, cluster = ~fips)
r2 <- feols(as.formula(paste0("f_lfp ~ conservative * wealth_bin + ", ctrl,
                              " | state_f + year_f + decile_f")),
            data = qd, weights = ~f_HHWT, cluster = ~fips)
r3 <- feols(as.formula(paste0("f_lfp ~ conservative * wealth_bin + ", ctrl,
                              " | state_f + year_f + decile_f^year_f")),
            data = qd, weights = ~f_HHWT, cluster = ~fips)

# r4 deliberately uses the CONTAMINATED couple-income decile, to show how much
# of the "wealth effect" is manufactured by conditioning on the outcome.
r4 <- feols(as.formula(paste0("f_lfp ~ conservative * wealth_bin + ", ctrl,
                              " | state_f + year_f + bad_decile_f")),
            data = qd[!is.na(bad_decile_f)], weights = ~f_HHWT, cluster = ~fips)

etable(r1, r2, r3, r4,
       headers = c("linear income ctrl", "+ husband-decile FE",
                   "+ husb-decile x year FE", "BAD CONTROL: couple-decile FE"),
       keep = c("conservative", "wealth_bin"), digits = 4)

# Age is the other composition threat: wealthy couples are ~5 years older and
# LFP falls steeply with age, so the raw quadrant conflates the two.
message("\nWife's LFP by wealth cell WITHIN age band (household-weighted, %):")
ab <- dcast(quad[!is.na(ageband), .(lfp = 100 * wmean(f_lfp, f_HHWT)),
                 by = .(ageband, wealthy)], ageband ~ wealthy, value.var = "lfp")
ab[, wealth_gap_pp := round(Wealthy - `Not wealthy`, 2)]
print(ab)
fwrite(ab, dated_path(results_dir, "t2_wealth_gap_by_age_band.csv"))

rob_out <- rbindlist(lapply(
  list("linear income ctrl" = r1, "+ decile FE" = r2, "+ decile x year FE" = r3),
  function(m) {
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    setnames(ct, c("term", "estimate", "std_error", "t_value", "p_value"))
    ct[term %like% "conservative|wealth_bin"]
  }), idcol = "model")
fwrite(rob_out, dated_path(results_dir, "t2_pooled_robustness.csv"))

message("
T2 complete. Tables and figures written to ", results_dir, " and data/graphs/.")
