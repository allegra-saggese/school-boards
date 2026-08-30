library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)
library(sandwich)
library(lmtest)

source("functions.R")
source("R/paths.R")

# =========================================================
# Part 1 summary outputs: descriptive tables and presentation figures
#
# The replication script produces the regression coefficients and BKP's two
# figures. This produces everything else a paper or talk needs:
#
#   1. Sample construction table  -- records -> couples, with attrition
#   2. Summary statistics by era  -- the conventional "Table 1"
#   3. Cliff ratio by year        -- has the threshold signature weakened?
#   4. Coefficient plot           -- beta1 across samples, with CIs
#   5. Coefficient BY YEAR        -- beta1 over time, with CIs
#
# (3) and (5) are the substantive additions: the era-bucket regressions compress
# 55 years into three numbers, which hides whether change was gradual, abrupt,
# or non-monotonic.
# =========================================================

results_dir <- data_path("processed", "results")
ensure_dir(results_dir)
sqlite_path <- data_path("interim", "ipums_bkp.sqlite")
con <- dbConnect(SQLite(), sqlite_path); on.exit(dbDisconnect(con), add = TRUE)

r_main <- setDT(dbGetQuery(con, "SELECT YEAR, MIN(rowid) lo, MAX(rowid) hi, COUNT(*) n
                                 FROM ipums_table GROUP BY YEAR")); setkey(r_main, YEAR)
all_years <- c(1970L, 1980L, 1990L, 2000L, 2001L:2024L)

nas <- function(x) fifelse(x >= 999999, NA_real_, as.numeric(x))
labor_income <- function(w,b,f,b0){ w<-nas(w); b0<-nas(b0); b<-nas(b); f<-nas(f)
  se <- fifelse(!is.na(b0), b0, rowSums(cbind(b,f), na.rm=TRUE))
  pmax(rowSums(cbind(w,se), na.rm=TRUE), 0) }
educ5 <- function(e) fcase(is.na(e),NA_character_, e<=5,"<HS", e==6,"HS",
                           e %in% 7:9,"Some college", e==10,"College", e==11,">College",
                           default=NA_character_)
race3 <- function(r,h) fcase(is.na(r)|is.na(h),NA_character_, h %in% 1:4,"Hispanic",
                             h==0 & r==1,"White", h==0 & r==2,"Black", default=NA_character_)

# ── Build couples for one year, tracking attrition ───────────────────────
build_year <- function(yr, track = FALSE) {
  a <- r_main[J(yr)]
  side <- function(sex, extra = "") setDT(dbGetQuery(con, paste0(
    "SELECT SAMPLE,SERIAL,PERNUM,SPLOC,HHWT,AGE,RACE,HISPAN,EDUC,EMPSTAT,UHRSWORK,
            INCWAGE,INCBUS,INCFARM,INCBUS00,STATEICP
     FROM ipums_table NOT INDEXED
     WHERE rowid BETWEEN ", a$lo, " AND ", a$hi, " AND YEAR = ", yr,
    " AND SEX = ", sex, extra)))
  n_all <- a$n
  w <- side(2); h <- side(1)
  n_female <- nrow(w)
  w <- w[AGE %between% c(18,65)]; h <- h[AGE %between% c(18,65)]
  n_age <- nrow(w)
  w <- w[SPLOC > 0]; h <- h[SPLOC > 0]
  n_sploc <- nrow(w)
  if (yr == 1970L) { w <- w[SAMPLE %in% c(197001,197002)]; h <- h[SAMPLE %in% c(197001,197002)]
                     w[, HHWT := HHWT/2] }
  w[, lab := labor_income(INCWAGE,INCBUS,INCFARM,INCBUS00)]
  h[, lab := labor_income(INCWAGE,INCBUS,INCFARM,INCBUS00)]
  p <- merge(w[, .(SAMPLE,SERIAL,HHWT,f_pn=PERNUM,f_sp=SPLOC,f_age=AGE,f_educ=EDUC,
                   f_race=RACE,f_hisp=HISPAN,f_emp=EMPSTAT,f_hrs=UHRSWORK,f_lab=lab,ST=STATEICP)],
             h[, .(SAMPLE,SERIAL,m_pn=PERNUM,m_sp=SPLOC,m_age=AGE,m_educ=EDUC,
                   m_emp=EMPSTAT,m_hrs=UHRSWORK,m_lab=lab)],
             by = c("SAMPLE","SERIAL"), allow.cartesian = TRUE)
  p <- p[f_sp == m_pn & m_sp == f_pn]
  n_pair <- nrow(p)
  p <- p[m_emp == 1]
  n_husbwork <- nrow(p)
  p[, YEAR := yr]
  if (track) attr(p, "attr") <- data.table(YEAR=yr, records=n_all, women=n_female,
    age_18_65=n_age, spouse_linked=n_sploc, mutual_pair=n_pair, husband_working=n_husbwork)
  p
}

message("Building couples 1970-2024 ...")
pieces <- lapply(all_years, function(y) { message("  ", y); build_year(y, track = TRUE) })
attrition <- rbindlist(lapply(pieces, function(x) attr(x, "attr")))
cp <- rbindlist(pieces, use.names = TRUE); rm(pieces); invisible(gc(verbose=FALSE))

cp[, z := fifelse((f_lab + m_lab) > 0, f_lab/(f_lab+m_lab), NA_real_)]
cp[, wife_lfp := as.integer(f_emp %in% c(1,2))]
cp[, wife_emp := as.integer(f_emp == 1)]
cp[, era := fcase(YEAR <= 2011, "BKP era (1970-2011)", default = "Post-BKP (2012-2024)")]

# ── 1) Sample construction ────────────────────────────────────────────────
fwrite(attrition, file.path(results_dir, "bkp_pure_sample_construction.csv"))
message("\n=== 1. SAMPLE CONSTRUCTION (selected years) ===")
print(attrition[YEAR %in% c(1970,1980,1990,2000,2010,2020,2024)])
tot <- attrition[, lapply(.SD, sum), .SDcols = setdiff(names(attrition),"YEAR")]
message("  TOTAL across all years:")
print(tot)

# ── 2) Summary statistics by era ──────────────────────────────────────────
sumstat <- cp[, .(
  couples            = .N,
  wife_age           = round(weighted.mean(f_age, HHWT), 1),
  husband_age        = round(weighted.mean(m_age, HHWT), 1),
  wife_college_pct   = round(100*weighted.mean(f_educ >= 10, HHWT, na.rm=TRUE), 1),
  husb_college_pct   = round(100*weighted.mean(m_educ >= 10, HHWT, na.rm=TRUE), 1),
  wife_lfp_pct       = round(100*weighted.mean(wife_lfp, HHWT), 1),
  wife_hours         = round(weighted.mean(f_hrs, HHWT, na.rm=TRUE), 1),
  husb_hours         = round(weighted.mean(m_hrs, HHWT, na.rm=TRUE), 1),
  wife_labinc        = round(weighted.mean(f_lab, HHWT)),
  husb_labinc        = round(weighted.mean(m_lab, HHWT)),
  wife_share_mean    = round(weighted.mean(z, HHWT, na.rm=TRUE), 3),
  wife_outearns_pct  = round(100*weighted.mean(z > 0.5, HHWT, na.rm=TRUE), 1)
), by = era][order(era)]
fwrite(sumstat, file.path(results_dir, "bkp_pure_summary_statistics.csv"))
message("\n=== 2. SUMMARY STATISTICS BY ERA ===")
print(t(sumstat))

# ── 3) Cliff ratio by year ────────────────────────────────────────────────
cliff_of <- function(z, wt, donut = 0.02) {
  b <- sum(wt[z >= 0.40 & z <  0.5-donut], na.rm=TRUE)
  a <- sum(wt[z >  0.5+donut & z <= 0.60], na.rm=TRUE)
  if (!is.finite(a) || a <= 0) return(NA_real_); b/a
}
# bootstrap CI so the series has error bands
cliff_ci <- function(d, B = 60) {
  est <- cliff_of(d$z, d$HHWT)
  bs <- replicate(B, { i <- sample.int(nrow(d), replace = TRUE)
                       cliff_of(d$z[i], d$HHWT[i]) })
  c(est = est, lo = unname(quantile(bs, .025, na.rm=TRUE)), hi = unname(quantile(bs, .975, na.rm=TRUE)))
}
set.seed(42)
cliff_yr <- rbindlist(lapply(all_years, function(y) {
  d <- cp[YEAR == y & !is.na(z) & z > 0 & z < 1]
  if (nrow(d) > 200000) d <- d[sample(.N, 200000)]
  c1 <- cliff_ci(d)
  data.table(YEAR = y, cliff = c1[["est"]], lo = c1[["lo"]], hi = c1[["hi"]], n = nrow(d))
}))
fwrite(cliff_yr, file.path(results_dir, "bkp_pure_cliff_ratio_by_year.csv"))
message("\n=== 3. CLIFF RATIO BY YEAR ===")
print(cliff_yr[, .(YEAR, cliff = round(cliff,3), lo = round(lo,3), hi = round(hi,3))])

p_cliff <- ggplot(cliff_yr, aes(x = YEAR, y = cliff)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.18) +
  geom_line(colour = "steelblue4", linewidth = 0.9) +
  geom_point(colour = "steelblue4", size = 1.8) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = 2011.5, linetype = "dotted", colour = "grey30") +
  annotate("text", x = 2012.5, y = max(cliff_yr$hi, na.rm=TRUE),
           label = "BKP sample ends", hjust = 0, size = 3, colour = "grey30") +
  labs(title = "Threshold avoidance over time",
       subtitle = paste0("Couples just below the equal-earnings threshold relative to just above ",
                         "([0.40,0.48) vs (0.52,0.60], ±2pp donut excluded).\n",
                         "1.0 = no avoidance. Shaded band = bootstrap 95% CI."),
       x = NULL, y = "Below / above ratio") +
  theme_minimal(base_size = 11)
save_plot("bkp_pure_cliff_ratio_by_year.png", { print(p_cliff) }, width = 2000, height = 1200)

message("\nwrote: sample construction, summary statistics, cliff-by-year (csv + png)")

# ── 4) Coefficient plot: beta1 across samples, with CIs ───────────────────
# Reads the coefficients the replication already produced rather than refitting.
coef_f <- file.path(results_dir, "bkp_pure_table23_era_comparison.csv")
if (file.exists(coef_f)) {
  cf <- fread(coef_f)
  cf[, `:=`(lo = beta1 - 1.96*se, hi = beta1 + 1.96*se)]
  cf[, spec := fcase(grepl("col 1", outcome), "Col 1: linear",
                     grepl("col 2", outcome), "Col 2: cubic",
                     grepl("col 4", outcome), "Col 4: cubic + children",
                     default = "other")]
  cf[, dv := fifelse(grepl("Wife LFP", outcome), "Wife's LFP", "Income gap")]
  cf[, era_short := fcase(grepl("BKP era", era), "(a) Replication\n1970-2011",
                          grepl("Post-BKP", era), "(b) Post-BKP\n2012-2024",
                          grepl("UPDATED", era), "(c) Updated\n1970-2024", default = era)]
  # BKP's published values for reference lines
  bkp_ref <- data.table(dv = c("Wife's LFP","Wife's LFP","Wife's LFP",
                               "Income gap","Income gap","Income gap"),
                        spec = rep(c("Col 1: linear","Col 2: cubic","Col 4: cubic + children"), 2),
                        published = c(-0.178, -0.142, -0.143, -0.031, -0.095, -0.109))
  cf <- merge(cf, bkp_ref, by = c("dv","spec"), all.x = TRUE)

  p_coef <- ggplot(cf, aes(x = era_short, y = beta1, colour = spec)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_hline(aes(yintercept = published, colour = spec), linetype = "dashed", alpha = 0.55) +
    geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.55), size = 0.55) +
    facet_wrap(~dv, scales = "free_y") +
    scale_colour_manual(values = c("Col 1: linear" = "#d73027",
                                   "Col 2: cubic" = "#4575b4",
                                   "Col 4: cubic + children" = "#1a9850"), name = NULL) +
    labs(title = "Effect of PrWifeEarnsMore, by sample and specification",
         subtitle = paste0("Points with 95% CIs. Dashed lines = BKP's published estimates.\n",
                           "Note the linear specification (red) does not replicate; the cubic does."),
         x = NULL, y = expression(beta[1])) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  save_plot("bkp_pure_coefficient_plot.png", { print(p_coef) }, width = 2200, height = 1200)
  message("wrote: coefficient plot")
}

# ── 5) beta1 BY YEAR, with CIs ────────────────────────────────────────────
# The era buckets compress 55 years into three numbers. Estimating the same
# specification separately by year shows whether the change was gradual or
# abrupt, and is the natural visual for "has the norm weakened".
probs <- seq(.05,.95,.05); pc <- paste0("p", 1:19)
wq <- function(x,w,p){ ok <- is.finite(x)&is.finite(w)&w>0; x<-x[ok]; w<-w[ok]
  if(!length(x)) return(rep(NA_real_,length(p)))
  o<-order(x); x<-x[o]; w<-w[o]; cw<-cumsum(w)/sum(w)
  sapply(p, function(q){ i<-which(cw>=q)[1]; if(is.na(i)) NA_real_ else x[i] }) }
a5 <- function(a) cut(a, seq(15,70,5), right=FALSE,
                      labels=paste0(seq(15,65,5),"-",seq(19,69,5)))

beta_by_year <- function(yr, nmax = 120000L) {
  d <- cp[YEAR == yr]
  if (!nrow(d)) return(NULL)
  d[, `:=`(age5=a5(f_age), e5=educ5(f_educ), r3=race3(f_race,f_hisp))]
  d <- d[!is.na(age5) & !is.na(e5) & !is.na(r3)]
  a <- r_main[J(yr)]
  ww <- setDT(dbGetQuery(con, paste0(
    "SELECT AGE,EDUC,RACE,HISPAN,STATEICP,SAMPLE,HHWT,INCWAGE,INCBUS,INCFARM,INCBUS00
     FROM ipums_table NOT INDEXED WHERE rowid BETWEEN ", a$lo, " AND ", a$hi,
    " AND YEAR = ", yr, " AND SEX = 2 AND AGE BETWEEN 18 AND 65 AND EMPSTAT = 1")))
  if (yr == 1970L) { ww <- ww[SAMPLE %in% c(197001,197002)]; ww[, HHWT := HHWT/2] }
  ww[, lab := labor_income(INCWAGE,INCBUS,INCFARM,INCBUS00)]
  ww <- ww[lab > 0]
  ww[, `:=`(age5=a5(AGE), e5=educ5(EDUC), r3=race3(RACE,HISPAN))]
  ww <- ww[!is.na(age5) & !is.na(e5) & !is.na(r3)]
  lk <- ww[, { if (sum(HHWT) < 30) as.list(setNames(rep(NA_real_,19), pc))
               else as.list(setNames(wq(lab, HHWT, probs), pc)) },
           by = .(STATEICP, age5, e5, r3)]
  d <- merge(d, lk, by.x=c("ST","age5","e5","r3"), by.y=c("STATEICP","age5","e5","r3"), all.x=TRUE)
  d <- d[complete.cases(d[, ..pc])]
  if (nrow(d) < 5000) return(NULL)
  M <- as.matrix(d[, ..pc]); d[, Pr := rowMeans(M > m_lab)]
  d[, lnh := log1p(pmax(m_lab, 0))]
  if (nrow(d) > nmax) d <- d[sample(.N, nmax)]
  f <- as.formula(paste0("wife_lfp ~ Pr + ", paste(pc, collapse=" + "),
                         " + lnh + I(lnh^2) + I(lnh^3) + factor(ST) + r3 + age5 + e5"))
  fit <- lm(f, data = d, weights = HHWT)
  s <- summary(fit)$coefficients
  data.table(YEAR = yr, beta1 = s["Pr","Estimate"], se = s["Pr","Std. Error"], n = nobs(fit))
}

message("\n=== 5. beta1 BY YEAR (cubic spec) ===")
by_year <- rbindlist(lapply(all_years, function(y) { message("  ", y); beta_by_year(y) }))
by_year[, `:=`(lo = beta1 - 1.96*se, hi = beta1 + 1.96*se)]
fwrite(by_year, file.path(results_dir, "bkp_pure_beta1_by_year.csv"))
print(by_year[, .(YEAR, beta1 = round(beta1,3), lo = round(lo,3), hi = round(hi,3), n)])

p_by_year <- ggplot(by_year, aes(x = YEAR, y = beta1)) +
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
  theme_minimal(base_size = 11)
save_plot("bkp_pure_beta1_by_year.png", { print(p_by_year) }, width = 2100, height = 1200)
message("\nAll Part 1 summary outputs written.")
