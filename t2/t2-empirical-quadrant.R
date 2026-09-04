# =============================================================================
# T2 — the culture x wealth quadrant
#
# Inputs : data/processed/panel/*_lfpr_panel_with_groups.csv
#          data/interim/ipums_bkp.sqlite
#          housing lookup from ipums-build-housing-merge.R
# Outputs: data/processed/results/YYYY-MM-DD_t2_*.csv
#          RDS cache at $T2_CACHE_WRITE, read by t2-figures.R via $T2_CACHE
#
# EXPLORATORY by design — this identifies no causal effect. It asks whether a
# breadwinner norm is visible at all, and if so whether its strength is elastic
# to WEALTH or to CULTURE. Married couples are sorted into a 2x2 and the wife's
# labour supply compared across cells, on both margins (does she participate;
# given participation, how many hours):
#
#                        not wealthy          wealthy
#     conservative   |  cons / poor      |  cons / wealthy  |
#     progressive    |  prog / poor      |  prog / wealthy  |
#
# PROXIES
#   culture : county presidential vote margin
#   wealth  : housing — house value, and ownership free of a mortgage
#
# WHY HOUSING IS THE WEALTH AXIS. Home equity is the median household's
# dominant asset, ownership is reported for ~70% of households, and VALUEH is a
# LEVEL rather than a binary "reports something". It also separates two forces
# with OPPOSITE SIGNS that a single asset-income measure blends into an
# uninformative null:
#   LEVERAGE — an expensive house carries a mortgage payment, which requires a
#              second income. Pushes her INTO the labour force.
#   WEALTH   — owning free and clear needs no debt service. Lets her OUT.
# INCINVST is retained in Section 6 as one robustness row.
#
# TWO GUARDS, without which the quadrant silently re-measures the income
# gradient T1 already estimated:
#   (a) house value is ranked WITHIN STATE x YEAR. A national dollar cut would
#       encode geography as wealth — and geography is also the culture axis,
#       manufacturing the very interaction being tested for.
#   (b) every regression conditions on the HUSBAND's labour-income decile,
#       never the couple's. Couple income contains HER earnings, so ranking on
#       it conditions on the dependent variable. Section 6 keeps the
#       contaminated specification alongside so the bias stays visible.
#
# LIMITATIONS
#   * Vote margin is CONTEXTUAL — the politics of the place, not the couple.
#     Cell assignment is measured with error, so culture estimates are
#     attenuated. State FE absorb the coarsest confounding.
#   * County is identified for only ~61% of ACS households, skewed large/urban.
#   * Repeated cross-sections; no couple is observed twice.
#   * Wealth and labour supply are a joint household choice.
#   * Housing data (extract usa:6) covers 2012-2020, matching the political
#     window that already bounds T2.
#
# The Bazzi et al. frontier-culture measure was removed from the project on
# 2026-08-30. Do not reintroduce it here.
# =============================================================================

library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)
library(fixest)

source(here::here("_setup.R"))

# ── 0) Config ────────────────────────────────────────────────────────────────
t2_years <- 2012:2020     # bounded by county presidential vote data
age_lo   <- 18L
age_hi   <- 65L
donut_primary <- 0.02     # matches t2/t2-rdd-breadwinner-norm.R
rdd_bw        <- 0.20

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)
sqlite_path <- data_path("interim", "ipums_bkp.sqlite")

# ── 1) Helpers ───────────────────────────────────────────────────────────────
# IPUMS codes "not applicable"/"missing" as large sentinels, not NA. Left in
# place they would be summed as if they were dollars.
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

# Persistent by default so t2-figures.R finds it with no environment set up;
# T2_CACHE_WRITE overrides.
cache_path <- Sys.getenv("T2_CACHE_WRITE", "")
if (!nzchar(cache_path)) cache_path <- data_path("interim", "t2_quadrant.rds")
cache_env  <- Sys.getenv("T2_CACHE", "")
use_cache  <- nzchar(cache_env) && file.exists(cache_env)

if (use_cache) {
  message("Reusing cached T2 sample: ", cache_env)
  quad <- readRDS(cache_env)
} else {

# ── 2) Pull the couples ──────────────────────────────────────────────────────
# NOT INDEXED is deliberate. The only index on this table is (AGE, SEX); with an
# AGE predicate SQLite otherwise picks it and turns a scan into ~10^8 index
# seeks, which measured 290x slower on this database.
message("Pulling ", min(t2_years), "-", max(t2_years), " spouse-linked adults ...")

con <- dbConnect(SQLite(), sqlite_path)
on.exit(dbDisconnect(con), add = TRUE)

persons <- setDT(dbGetQuery(con, sprintf("
  SELECT YEAR, SAMPLE, SERIAL, PERNUM, SPLOC, SEX, AGE, STATEICP, COUNTYICP,
         HHWT, EDUC, RACE, HISPAN, NCHILD, LABFORCE, EMPSTAT,
         UHRSWORK, WKSWORK1,
         INCWAGE, INCBUS, INCBUS00, INCFARM, INCTOT, INCINVST
  FROM ipums_table NOT INDEXED
  WHERE YEAR BETWEEN %d AND %d
    AND AGE BETWEEN %d AND %d
    AND SPLOC > 0
    AND COUNTYICP > 0",
  min(t2_years), max(t2_years), age_lo, age_hi)))

message("  spouse-linked adults ", age_lo, "-", age_hi, " with county: ",
        format(nrow(persons), big.mark = ","))

# ── 3) Build opposite-sex couples on a MUTUAL spouse link ────────────────────
# SPLOC holds the spouse's PERNUM. Requiring the link to point back
# (m.PERNUM == f.SPLOC AND m.SPLOC == f.PERNUM) drops the one-directional links
# that arise in complex households.
wives    <- persons[SEX == 2L]
husbands <- persons[SEX == 1L]
idc <- c("YEAR", "SAMPLE", "SERIAL")
setnames(wives,    setdiff(names(wives),    idc), paste0("f_", setdiff(names(wives),    idc)))
setnames(husbands, setdiff(names(husbands), idc), paste0("m_", setdiff(names(husbands), idc)))

pairs <- merge(wives, husbands,
               by.x = c(idc, "f_SPLOC"), by.y = c(idc, "m_PERNUM"), all = FALSE)
setnames(pairs, "f_SPLOC", "m_PERNUM")
pairs <- pairs[m_SPLOC == f_PERNUM]
rm(persons, wives, husbands); invisible(gc())
message("  mutually-linked opposite-sex couples: ", format(nrow(pairs), big.mark = ","))

# ── 4) Variables ─────────────────────────────────────────────────────────────
# 4a. Labour supply -- the two outcome margins.
# LABFORCE: 1 = not in labour force, 2 = in labour force.
pairs[, f_lfp := fifelse(f_LABFORCE %in% c(1L, 2L), as.numeric(f_LABFORCE == 2L), NA_real_)]
# UHRSWORK uses 0 as "not applicable", which for a non-worker is a true zero of
# hours. Zeros stay in the unconditional measure, out of the conditional one.
pairs[, f_weekly_hours     := fifelse(is.na(f_UHRSWORK), NA_real_, as.numeric(f_UHRSWORK))]
pairs[, f_hours_if_working := fifelse(f_weekly_hours > 0, f_weekly_hours, NA_real_)]

# 4b. Earnings.
pairs[, f_labinc := labor_income(f_INCWAGE, f_INCBUS, f_INCFARM, f_INCBUS00)]
pairs[, m_labinc := labor_income(m_INCWAGE, m_INCBUS, m_INCFARM, m_INCBUS00)]
pairs[, couple_labinc := f_labinc + m_labinc]

# 4c. Asset income -- retained ONLY as a robustness row in Section 6.
pairs[, couple_assetinc := rowSums(cbind(pmax(nas(f_INCINVST), 0),
                                         pmax(nas(m_INCINVST), 0)), na.rm = TRUE)]
pairs[, wealthy_asset := as.numeric(couple_assetinc > 0)]

# 4d. CULTURE AXIS -- county vote margin.
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

# 4e. WEALTH AXIS -- housing (IPUMS extract usa:6, one row per household).
hw <- fread(file.path(panel_dir, "housing_wealth_by_household.csv"))
pairs <- merge(pairs, hw, by = c("YEAR", "SAMPLE", "SERIAL"), all.x = TRUE)
message("  couples matched to a housing record: ",
        format(sum(!is.na(pairs$owns)), big.mark = ","),
        " (", round(100 * mean(!is.na(pairs$owns)), 1), "%)")

quad <- pairs[!is.na(culture)]
rm(pairs); invisible(gc())
saveRDS(quad, cache_path)
message("  cached sample for re-runs: ", cache_path)
}

# ── 5) Derived axes and controls (applied on both the cold and cached path) ──
quad[, conservative := as.numeric(culture == "Conservative")]

# House value ranked WITHIN STATE x YEAR -- see header guard (a).
quad[, home_value_rank := frank(home_value, ties.method = "average", na.last = "keep") /
                          sum(!is.na(home_value)), by = .(f_STATEICP, YEAR)]
quad[, wealthy := fifelse(is.na(owns), NA_real_,
                   fifelse(owns == 1L & !is.na(home_value_rank) & home_value_rank >= 0.75, 1, 0))]
quad[, outright := as.numeric(owns_outright)]
# Renters are coded NOT wealthy rather than missing: renting at these ages is
# itself a wealth signal, not an absence of information.
quad[, wealth_tier := fcase(
  is.na(owns),                    NA_character_,
  owns == 0L,                     "1 Renter",
  owns == 1L & outright %in% 0,   "2 Owner, mortgaged",
  owns == 1L & outright %in% 1 & (is.na(home_value_rank) | home_value_rank < 0.75),
                                  "3 Owner outright",
  owns == 1L & outright %in% 1,   "4 Owner outright, top-quartile value",
  default = NA_character_)]

# Husband's labour-income decile, ranked within year -- see header guard (b).
quad[m_labinc > 0,
     husb_decile := as.integer(pmin(10, floor(10 * (frank(m_labinc, ties.method = "average") - 0.5) / .N) + 1)),
     by = YEAR]
# The contaminated couple-income version, kept ONLY for the Section 6 contrast.
quad[couple_labinc > 0,
     bad_decile := as.integer(pmin(10, floor(10 * (frank(couple_labinc, ties.method = "average") - 0.5) / .N) + 1)),
     by = YEAR]

# EDUC (IPUMS): <=6 is 12th grade or less, 7-9 some college, >=10 a 4-year degree.
educ3 <- function(e) fcase(is.na(e) | e == 0L, NA_character_,
                           e <= 6L,  "1 HS or less",
                           e <= 9L,  "2 Some college",
                           default = "3 College+")
quad[, `:=`(f_educ3 = educ3(f_EDUC), m_educ3 = educ3(m_EDUC))]

# NCHILD is the count of own children in the household. Kept as a count (for
# controls) and as a capped factor (for the decomposition), since the labour
# supply response is steeply non-linear in the first child.
quad[, nchild    := as.numeric(f_NCHILD)]
quad[, nchild_f  := factor(pmin(nchild, 3), levels = 0:3,
                           labels = c("0", "1", "2", "3+"))]
quad[, any_kids  := as.numeric(nchild > 0)]

quad[, `:=`(
  year_f      = factor(YEAR),
  state_f     = factor(f_STATEICP),
  decile_f    = factor(husb_decile),
  f_age       = as.numeric(f_AGE),
  m_age       = as.numeric(m_AGE),
  f_college   = as.numeric(f_EDUC >= 10L),
  m_college   = as.numeric(m_EDUC >= 10L),
  ln_m_labinc = log1p(pmax(m_labinc, 0))
)]

# Control set. nchild enters as a count, not a binary: the participation
# response to a second and third child is not the same as to the first.
ctrl <- paste("f_age + I(f_age^2) + m_age + f_college + m_college +",
              "nchild + I(nchild^2) + ln_m_labinc")

qa <- quad[!is.na(wealthy)]
message("\nAnalysis sample (culture + housing wealth both defined): ",
        format(nrow(qa), big.mark = ","))

# ── 6) The quadrant ──────────────────────────────────────────────────────────
message("\n=== T2.1  THE QUADRANT: wife's labour supply by culture x wealth ===")

quad_tab <- qa[, .(couples          = .N,
                   lfp_pct          = round(100 * wmean(f_lfp, f_HHWT), 2),
                   weekly_hours     = round(wmean(f_weekly_hours, f_HHWT), 2),
                   hours_if_working = round(wmean(f_hours_if_working, f_HHWT), 2),
                   mean_nchild      = round(wmean(nchild, f_HHWT), 2)),
               by = .(culture, wealthy)][order(culture, wealthy)]
print(quad_tab)
fwrite(quad_tab, dated_path(results_dir, "t2_quadrant_means.csv"))

message("\nWife's labour supply by housing tier (unconditional):")
tier_tab <- qa[!is.na(wealth_tier), .(
  couples      = .N,
  lfp_pct      = round(100 * wmean(f_lfp, f_HHWT), 2),
  weekly_hours = round(wmean(f_weekly_hours, f_HHWT), 2)
), by = wealth_tier][order(wealth_tier)]
print(tier_tab)
fwrite(tier_tab, dated_path(results_dir, "t2_housing_tier_means.csv"))

# ── 7) Regressions ───────────────────────────────────────────────────────────
# State FE matter because BOTH axes are geographic: without them a "conservative
# county" coefficient can pick up any way Alabama differs from Massachusetts.
# SEs clustered on county, the level at which the culture proxy varies.
message("\n=== T2.2  REGRESSIONS (state + year FE, SE clustered on county) ===")

m_lfp  <- feols(as.formula(paste0("f_lfp ~ conservative * wealthy + ", ctrl,
                                  " | state_f + year_f + decile_f")),
                data = qa[!is.na(decile_f)], weights = ~f_HHWT, cluster = ~fips)
m_hrs  <- feols(as.formula(paste0("f_weekly_hours ~ conservative * wealthy + ", ctrl,
                                  " | state_f + year_f + decile_f")),
                data = qa[!is.na(decile_f)], weights = ~f_HHWT, cluster = ~fips)
m_out  <- feols(as.formula(paste0("f_lfp ~ conservative * outright + ", ctrl,
                                  " | state_f + year_f + decile_f")),
                data = quad[!is.na(outright) & !is.na(decile_f)],
                weights = ~f_HHWT, cluster = ~fips)
m_asset <- feols(as.formula(paste0("f_lfp ~ conservative * wealthy_asset + ", ctrl,
                                   " | state_f + year_f + decile_f")),
                 data = quad[!is.na(decile_f)], weights = ~f_HHWT, cluster = ~fips)

etable(m_lfp, m_hrs, m_out, m_asset,
       headers = c("LFP, top-qtile home", "Hours, top-qtile home",
                   "LFP, owns outright", "LFP, asset income (robustness)"),
       keep = c("conservative", "wealthy", "outright", "wealthy_asset"), digits = 4)

# The bad-control contrast: same model, contaminated conditioning set.
m_bad <- feols(as.formula(paste0("f_lfp ~ conservative * wealthy + ", ctrl,
                                 " | state_f + year_f + bad_decile")),
               data = qa[!is.na(bad_decile)], weights = ~f_HHWT, cluster = ~fips)
message("\nBad-control contrast -- conditioning on COUPLE income (contains her earnings):")
etable(m_lfp, m_bad,
       headers = c("husband-decile FE (correct)", "couple-decile FE (BAD CONTROL)"),
       keep = c("conservative", "wealthy"), digits = 4)

fwrite(rbindlist(lapply(
  list("LFP top-quartile home"   = m_lfp,  "Hours top-quartile home" = m_hrs,
       "LFP owns outright"       = m_out,  "LFP asset income"        = m_asset,
       "LFP BAD CONTROL"         = m_bad),
  function(m) {
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    setnames(ct, c("term", "estimate", "std_error", "t_value", "p_value"))
    ct[term %like% "conservative|wealthy|outright"]
  }), idcol = "model"),
  dated_path(results_dir, "t2_regressions.csv"))

# ── 8) Is the norm income-elastic? Decile by decile ─────────────────────────
# An income-ELASTIC norm should be absent at the bottom of the husband's
# earnings distribution and present at the top. A fixed cultural constant would
# instead be flat across deciles.
message("\n=== T2.3  DECILE BY DECILE: is the norm income-elastic? ===")

dec_coefs <- rbindlist(lapply(sort(unique(qa[!is.na(husb_decile)]$husb_decile)), function(d) {
  sub <- qa[husb_decile == d]
  if (nrow(sub) < 5000L || uniqueN(sub$state_f) < 2L) return(NULL)
  m <- tryCatch(feols(as.formula(paste0("f_lfp ~ conservative * wealthy + ", ctrl,
                                        " | state_f + year_f")),
                      data = sub, weights = ~f_HHWT, cluster = ~fips),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "estimate", "std_error", "t_value", "p_value"))
  ct[term %in% c("conservative", "wealthy", "conservative:wealthy")][
     , `:=`(husb_decile = d, n = nrow(sub))]
}))
dec_coefs[, `:=`(ci_lo = estimate - 1.96 * std_error,
                 ci_hi = estimate + 1.96 * std_error)]
print(dec_coefs[, .(husb_decile, term, est_pp = round(100 * estimate, 2),
                    lo_pp = round(100 * ci_lo, 2), hi_pp = round(100 * ci_hi, 2),
                    p = signif(p_value, 2))][order(term, husb_decile)])
fwrite(dec_coefs, dated_path(results_dir, "t2_decile_coefficients.csv"))

# ── 9) Education decomposition ───────────────────────────────────────────────
# Education is the obvious alternative explanation for both axes: it predicts
# earnings capacity, marriage market position, AND stated gender attitudes. If
# the quadrant interaction is really an education effect, it should vanish once
# estimated within education group.
message("\n=== T2.4  EDUCATION DECOMPOSITION (wife's education) ===")

edu_tab <- qa[!is.na(f_educ3), .(
  couples = .N,
  lfp_pct = round(100 * wmean(f_lfp, f_HHWT), 2)
), by = .(f_educ3, culture, wealthy)][order(f_educ3, culture, wealthy)]
print(dcast(edu_tab, f_educ3 + wealthy ~ culture, value.var = "lfp_pct"))

edu_coefs <- rbindlist(lapply(sort(unique(na.omit(qa$f_educ3))), function(g) {
  sub <- qa[f_educ3 == g & !is.na(decile_f)]
  if (nrow(sub) < 5000L) return(NULL)
  m <- tryCatch(feols(as.formula(paste0("f_lfp ~ conservative * wealthy + ", ctrl,
                                        " | state_f + year_f + decile_f")),
                      data = sub, weights = ~f_HHWT, cluster = ~fips),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "estimate", "std_error", "t_value", "p_value"))
  ct[term %in% c("conservative", "wealthy", "conservative:wealthy")][
     , `:=`(f_educ3 = g, n = nrow(sub))]
}))
message("\nLFP effects by wife's education (pp):")
print(edu_coefs[, .(f_educ3, term, est_pp = round(100 * estimate, 2),
                    se_pp = round(100 * std_error, 2), p = signif(p_value, 2))][order(term, f_educ3)])
fwrite(edu_coefs, dated_path(results_dir, "t2_education_decomposition.csv"))

# ── 10) Children decomposition ──────────────────────────────────────────────
# The norm may operate THROUGH fertility rather than directly on labour supply:
# if conservative-and-wealthy couples simply have more children, the quadrant
# interaction could be a childcare constraint wearing a cultural label.
message("\n=== T2.5  CHILDREN ===")

message("\nDoes the quadrant differ in family size? (mean own children)")
print(dcast(qa[, .(nchild = round(wmean(nchild, f_HHWT), 2)), by = .(culture, wealthy)],
            culture ~ wealthy, value.var = "nchild"))

kid_coefs <- rbindlist(lapply(levels(qa$nchild_f), function(g) {
  sub <- qa[nchild_f == g & !is.na(decile_f)]
  if (nrow(sub) < 5000L) return(NULL)
  m <- tryCatch(feols(as.formula(paste0(
      "f_lfp ~ conservative * wealthy + f_age + I(f_age^2) + m_age + ",
      "f_college + m_college + ln_m_labinc | state_f + year_f + decile_f")),
      data = sub, weights = ~f_HHWT, cluster = ~fips), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "estimate", "std_error", "t_value", "p_value"))
  ct[term %in% c("conservative", "wealthy", "conservative:wealthy")][
     , `:=`(nchild = g, n = nrow(sub))]
}))
message("\nLFP effects by number of own children (pp):")
print(kid_coefs[, .(nchild, term, est_pp = round(100 * estimate, 2),
                    se_pp = round(100 * std_error, 2), p = signif(p_value, 2))][order(term, nchild)])
fwrite(kid_coefs, dated_path(results_dir, "t2_children_decomposition.csv"))

# ── 11) The donut kink RDD, with state fixed effects ────────────────────────
# t2/t2-rdd-breadwinner-norm.R Section 10 runs this with YEAR FE only and
# unclustered SEs. Both are corrected here: state FE force a within-state
# comparison, and clustering on county respects the level at which the culture
# proxy is assigned.
message("\n=== T2.6  DONUT KINK RDD WITH STATE FE ===")

rdd <- qa[f_labinc > 0 & m_labinc > 0]
rdd[, z := f_labinc / (f_labinc + m_labinc)]
rdd <- rdd[!is.na(z) & abs(z - 0.5) > donut_primary & abs(z - 0.5) <= rdd_bw]
rdd[, z_c  := z - 0.5]
rdd[, D    := as.integer(z_c >= 0)]
rdd[, D_zc := D * z_c]
message("  RDD sample (interior, donut-excluded, |z-0.5| <= ", rdd_bw, "): ",
        format(nrow(rdd), big.mark = ","), " couples")

f_pol   <- "~ (z_c + D + D_zc) * conservative"
r_yr    <- feols(as.formula(paste("f_weekly_hours", f_pol, "| year_f")),
                 data = rdd, weights = ~f_HHWT)
r_yr_cl <- feols(as.formula(paste("f_weekly_hours", f_pol, "| year_f")),
                 data = rdd, weights = ~f_HHWT, cluster = ~fips)
r_st    <- feols(as.formula(paste("f_weekly_hours", f_pol, "| year_f + state_f")),
                 data = rdd, weights = ~f_HHWT, cluster = ~fips)
r_w     <- feols(f_weekly_hours ~ (z_c + D + D_zc) * wealthy | year_f + state_f,
                 data = rdd, weights = ~f_HHWT, cluster = ~fips)

etable(r_yr, r_yr_cl, r_st, r_w,
       headers = c("year FE (original)", "+ cluster county", "+ STATE FE",
                   "wealth interaction"), digits = 4)

fwrite(rbindlist(lapply(
  list("year FE only" = r_yr, "year FE + clustered" = r_yr_cl,
       "+ state FE"   = r_st, "wealth interaction"  = r_w),
  function(m) {
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    setnames(ct, c("term", "estimate", "std_error", "t_value", "p_value"))
    ct
  }), idcol = "model", fill = TRUE),
  dated_path(results_dir, "t2_rdd_state_fe.csv"))

# ── 12) Figures ─────────────────────────────────────────────────────────────
save_plot("t2_quadrant_lfp_and_hours.png", {
  pd <- melt(quad_tab, id.vars = c("culture", "wealthy"),
             measure.vars = c("lfp_pct", "weekly_hours"),
             variable.name = "outcome", value.name = "value")
  pd[, outcome := factor(outcome, levels = c("lfp_pct", "weekly_hours"),
        labels = c("Wife's labour force participation (%)",
                   "Wife's usual weekly hours (incl. zeros)"))]
  pd[, wl := fifelse(wealthy == 1, "Top-quartile home value\n(within state x year)", "Other")]
  print(ggplot(pd, aes(x = wl, y = value, fill = culture)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = sprintf("%.1f", value)),
              position = position_dodge(width = 0.8), vjust = -0.4, size = 3.2) +
    facet_wrap(~outcome, scales = "free_y") +
    scale_fill_manual(values = c(Conservative = "#B2182B", Progressive = "#2166AC")) +
    labs(title = "T2: wife's labour supply by culture x wealth quadrant",
         subtitle = paste0("Married couples, both 18-65, ", min(t2_years), "-", max(t2_years),
                           ". Culture = county vote margin; wealth = housing.",
                           "\nUnconditional means, household-weighted."),
         x = NULL, y = NULL, fill = "County lean",
         caption = "Source: IPUMS USA extracts 4 and 6. Exploratory; see script header.") +
    theme_minimal(base_size = 12) + theme(legend.position = "top"))
}, width = 2200, height = 1200)

save_plot("t2_income_elasticity_of_the_norm.png", {
  pd <- copy(dec_coefs)
  pd[, term := factor(term, levels = c("wealthy", "conservative", "conservative:wealthy"),
        labels = c("Wealth (top-quartile home)", "Culture (conservative county)",
                   "Culture x wealth"))]
  print(ggplot(pd, aes(x = husb_decile, y = 100 * estimate)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_ribbon(aes(ymin = 100 * ci_lo, ymax = 100 * ci_hi), fill = "#4292C6", alpha = 0.25) +
    geom_line(colour = "#08519C", linewidth = 0.9) +
    geom_point(colour = "#08519C", size = 1.8) +
    facet_wrap(~term, nrow = 1, scales = "free_y") +
    scale_x_continuous(breaks = 1:10) +
    labs(title = "T2: is the breadwinner norm income-elastic?",
         subtitle = paste0("Effect on wife's LFP, estimated within each husband's ",
                           "labour-income decile, ", min(t2_years), "-", max(t2_years),
                           ".\nControls: both ages, both education, number of children, ",
                           "husband's labour income; state and year FE. 95% CI, SE clustered on county."),
         x = "Husband's labour-income decile (within year)",
         y = "Effect on wife's LFP (percentage points)") +
    theme_minimal(base_size = 12) + theme(panel.spacing = unit(1.2, "lines")))
}, width = 2600, height = 1100)

save_plot("t2_education_and_children_decomposition.png", {
  e <- edu_coefs[term == "conservative:wealthy",
                 .(group = f_educ3, facet = "By wife's education",
                   est = 100 * estimate, se = 100 * std_error)]
  k <- kid_coefs[term == "conservative:wealthy",
                 .(group = nchild, facet = "By number of own children",
                   est = 100 * estimate, se = 100 * std_error)]
  pd <- rbind(e, k)
  print(ggplot(pd, aes(x = group, y = est)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                  width = 0.12, colour = "#08519C") +
    geom_point(size = 2.6, colour = "#08519C") +
    facet_wrap(~facet, scales = "free_x") +
    labs(title = "T2: does the culture x wealth interaction survive decomposition?",
         subtitle = paste0("Interaction effect on wife's LFP, estimated separately within ",
                           "each group.\nIf the result were an education or family-size ",
                           "effect in disguise, it would vanish in some cells."),
         x = NULL, y = "Culture x wealth interaction (pp)") +
    theme_minimal(base_size = 12))
}, width = 2400, height = 1100)

message("\nT2 complete. Tables and figures written to ", results_dir, " and data/graphs/.")
