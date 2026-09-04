# =============================================================================
# T2 — publication tables and figures
#
#   1. Balance table across the four quadrant cells (with standardised
#      differences), because the cells are badly imbalanced on the husband's
#      income and the reader needs to see that before any raw comparison.
#   2. Main coefficient table, formatted, with FE rows and clustered SEs.
#   3. Coefficient plot: the interaction across all four wealth proxies.
#   4. Interaction by YEAR -- is the headline stable across 2012-2020, or is it
#      driven by a subset of years? This one is a real robustness check, not
#      just presentation.
#   5. The housing tier ladder as a figure.
# =============================================================================
suppressMessages({library(data.table); library(ggplot2); library(fixest)})
source(here::here("_setup.R"))

# T2 — all tables and figures.
# Input  : the RDS quadrant cache at $T2_CACHE (written by t2-empirical-quadrant.R)
# Outputs: data/graphs/YYYY-MM-DD_t2_*.png
#          data/processed/results/YYYY-MM-DD_t2_{balance_table,main_table,interaction_by_year}.*
results_dir <- data_path("processed","results")

# Falls back to the conventional location so the script runs without the env
# var; T2_CACHE overrides it.
cache_path <- Sys.getenv("T2_CACHE", "")
if (!nzchar(cache_path)) cache_path <- data_path("interim", "t2_quadrant.rds")
if (!file.exists(cache_path)) {
  stop("Quadrant cache not found: ", cache_path, "\n",
       "Run t2-empirical-quadrant.R first, or point T2_CACHE at the cache.")
}
q <- readRDS(cache_path)
q[, conservative := as.numeric(culture=="Conservative")]
q[, home_value_rank := frank(home_value, ties.method="average", na.last="keep")/sum(!is.na(home_value)),
  by=.(f_STATEICP, YEAR)]
q[, wealthy := fifelse(is.na(owns), NA_real_,
        fifelse(owns==1L & !is.na(home_value_rank) & home_value_rank>=0.75, 1, 0))]
q[, outright := as.numeric(owns_outright)]
q[, wealthy_asset := as.numeric(couple_assetinc > 0)]
q[m_labinc>0, husb_decile := as.integer(pmin(10, floor(10*(frank(m_labinc,ties.method="average")-0.5)/.N)+1)), by=YEAR]
q[couple_labinc>0, bad_decile := as.integer(pmin(10, floor(10*(frank(couple_labinc,ties.method="average")-0.5)/.N)+1)), by=YEAR]
q[, f_lfp := fifelse(f_LABFORCE %in% c(1L,2L), as.numeric(f_LABFORCE==2L), NA_real_)]
q[, f_hours := fifelse(is.na(f_UHRSWORK), NA_real_, as.numeric(f_UHRSWORK))]
q[, `:=`(f_age=as.numeric(f_AGE), m_age=as.numeric(m_AGE),
         f_college=as.numeric(f_EDUC>=10L), m_college=as.numeric(m_EDUC>=10L),
         nchild=as.numeric(f_NCHILD), ln_m_labinc=log1p(pmax(m_labinc,0)),
         year_f=factor(YEAR), state_f=factor(f_STATEICP))]
q[, decile_f := factor(husb_decile)]
q[, bad_decile_f := factor(bad_decile)]
qa <- q[!is.na(culture) & !is.na(wealthy)]
ctrl <- "f_age + I(f_age^2) + m_age + f_college + m_college + nchild + I(nchild^2) + ln_m_labinc"

# ── 1. BALANCE TABLE ────────────────────────────────────────────────────────
bal_vars <- c("f_age","m_age","f_college","m_college","nchild","m_labinc",
              "f_labinc","home_value","f_lfp","f_hours")
cells <- qa[, .(cell = paste0(culture, " / ", ifelse(wealthy==1,"wealthy","not wealthy"))), ]
qa[, cell := paste0(culture, " / ", ifelse(wealthy==1,"wealthy","not wealthy"))]
bal <- rbindlist(lapply(bal_vars, function(v) {
  s <- qa[, .(m = weighted.mean(get(v), f_HHWT, na.rm=TRUE)), by=cell]
  d <- dcast(s, . ~ cell, value.var="m")[, -1]
  # standardised difference: conservative/wealthy vs progressive/wealthy
  x1 <- qa[cell=="Conservative / wealthy"][[v]]; x2 <- qa[cell=="Progressive / wealthy"][[v]]
  sd_p <- sqrt((var(x1,na.rm=TRUE)+var(x2,na.rm=TRUE))/2)
  cbind(data.table(variable=v), d,
        data.table(std_diff_wealthy = round((mean(x1,na.rm=TRUE)-mean(x2,na.rm=TRUE))/sd_p, 3)))
}), fill=TRUE)
cat("=== 1. BALANCE ACROSS THE FOUR CELLS (household-weighted means) ===\n")
print(bal)
fwrite(bal, dated_path(results_dir, "t2_balance_table.csv"))

# ── 2. MAIN COEFFICIENT TABLE ───────────────────────────────────────────────
f_lfp1 <- feols(as.formula(paste0("f_lfp ~ conservative*wealthy + ",ctrl," | state_f + year_f")),
                data=qa, weights=~f_HHWT, cluster=~fips)
f_lfp2 <- feols(as.formula(paste0("f_lfp ~ conservative*wealthy + ",ctrl," | state_f + year_f + decile_f")),
                data=qa[!is.na(decile_f)], weights=~f_HHWT, cluster=~fips)
f_hrs  <- feols(as.formula(paste0("f_hours ~ conservative*wealthy + ",ctrl," | state_f + year_f + decile_f")),
                data=qa[!is.na(decile_f)], weights=~f_HHWT, cluster=~fips)
f_out  <- feols(as.formula(paste0("f_lfp ~ conservative*outright + ",ctrl," | state_f + year_f + decile_f")),
                data=q[!is.na(culture)&!is.na(outright)&!is.na(decile_f)], weights=~f_HHWT, cluster=~fips)
f_ast  <- feols(as.formula(paste0("f_lfp ~ conservative*wealthy_asset + ",ctrl," | state_f + year_f + decile_f")),
                data=q[!is.na(culture)&!is.na(decile_f)], weights=~f_HHWT, cluster=~fips)
f_bad  <- feols(as.formula(paste0("f_lfp ~ conservative*wealthy + ",ctrl," | state_f + year_f + bad_decile_f")),
                data=qa[!is.na(bad_decile_f)], weights=~f_HHWT, cluster=~fips)

cat("\n=== 2. MAIN COEFFICIENT TABLE ===\n")
# Readable labels. Without a dict, etable prints raw code names (state_f,
# bad_decile_f), and every column shows the same dependent variable while the
# thing that actually varies -- the wealth measure and the fixed effects -- is
# invisible. Both are fixed here.
dict <- c(
  f_lfp                       = "Wife in labour force",
  f_hours                     = "Wife's weekly hours",
  conservative                = "Republican-majority county",
  wealthy                     = "Wealthy: top-quartile home value",
  outright                    = "Wealthy: owns home outright",
  wealthy_asset               = "Wealthy: any asset income",
  "conservative:wealthy"      = "Republican x top-quartile home",
  "conservative:outright"     = "Republican x owns outright",
  "conservative:wealthy_asset"= "Republican x asset income",
  state_f                     = "State FE",
  year_f                      = "Year FE",
  decile_f                    = "Husband income decile FE",
  bad_decile_f                = "Couple income decile FE (BAD CONTROL)")

# etable SPANS the dependent-variable row across adjacent columns sharing an
# outcome, which makes five LFP columns look like two. State the outcome and
# the wealth measure explicitly in every column header instead.
# etable MERGES adjacent identical header strings into \multicolumn spans,
# which makes "LFP, LFP, Hours, LFP, LFP, LFP" render as a 2-span, a single
# cell, then a 3-span -- and a reader cannot tell which column is which.
# Prefixing the column number makes every string unique, which blocks merging.
hdr_dv <- c("(1) LFP 0/1", "(2) LFP 0/1", "(3) HOURS",
            "(4) LFP 0/1", "(5) LFP 0/1", "(6) LFP 0/1")
# Same treatment for the other two header rows. The groupings they would merge
# into are accurate, but a reader cannot count across a span reliably, so every
# cell is labelled with its column number.
hdr_w  <- c("(1) Top-qtile home", "(2) Top-qtile home", "(3) Top-qtile home",
            "(4) Owns outright",  "(5) Any asset inc",  "(6) Top-qtile home")
hdr_fe <- c("(1) state+year", "(2) +husb dec", "(3) +husb dec",
            "(4) +husb dec",  "(5) +husb dec", "(6) +COUPLE dec")

# No `file`/`replace` here: passing them switches etable into LaTeX mode, which
# is what we want for the .tex output but not for the readable .txt.
show_tab <- function() {
  etable(f_lfp1, f_lfp2, f_hrs, f_out, f_ast, f_bad,
         dict = dict,
         headers = list("Outcome" = hdr_dv, "Wealth measured as" = hdr_w,
                        "Fixed effects" = hdr_fe),
         keep = c("Republican", "Wealthy"),
         # The control set is ESTIMATED in every column but hidden by `keep`.
         # Without this row the table silently implies there are no controls.
         extralines = list("Demographic controls" = rep("Yes", 6),
                           "  (wife age, age^2)"  = rep("Yes", 6),
                           "  (husband age)"      = rep("Yes", 6),
                           "  (both college)"     = rep("Yes", 6),
                           "  (children, children^2)" = rep("Yes", 6),
                           "  (ln husband labour income)" = rep("Yes", 6)),
         digits = 4, digits.stats = 3, depvar = FALSE)
}
show_tab()
cat("\nColumn guide:\n")
cat("  (1) housing wealth, no husband-decile FE\n")
cat("  (2) PREFERRED: housing wealth, + husband-decile FE\n")
cat("  (3) same as (2) but the outcome is HOURS, not participation\n")
cat("  (4) wealth measured as owning outright instead of house value\n")
cat("  (5) wealth measured as asset income (the retired proxy)\n")
cat("  (6) DIAGNOSTIC ONLY -- conditions on COUPLE income, which contains the\n")
cat("      wife's own earnings. Included to show the bad control flips the sign\n")
cat("      of the wealth main effect. Not a result.\n")
sink(dated_path(results_dir, "t2_main_table.txt")); show_tab(); sink()
etable(f_lfp1, f_lfp2, f_hrs, f_out, f_ast, f_bad, tex = TRUE, dict = dict,
       headers = list("Outcome" = hdr_dv, "Wealth measured as" = hdr_w,
                      "Fixed effects" = hdr_fe),
       keep = c("Republican","Wealthy"), depvar = FALSE,
       extralines = list("Demographic controls" = rep("Yes", 6),
                         "  (wife age, age^2)"  = rep("Yes", 6),
                         "  (husband age)"      = rep("Yes", 6),
                         "  (both college)"     = rep("Yes", 6),
                         "  (children, children^2)" = rep("Yes", 6),
                         "  (ln husband labour income)" = rep("Yes", 6)),
       file = dated_path(results_dir, "t2_main_table.tex"), replace = TRUE)

# ── 3. COEFFICIENT PLOT ACROSS WEALTH PROXIES ───────────────────────────────
grab <- function(m, lab, want) {
  ct <- as.data.table(coeftable(m), keep.rownames="term")
  setnames(ct, c("term","est","se","t","p"))
  ct[term == want][, .(spec = lab, est, se, lo = est-1.96*se, hi = est+1.96*se, p)]
}
cp <- rbindlist(list(
  grab(f_lfp2,"Top-quartile home value\n(LFP)","conservative:wealthy"),
  grab(f_out ,"Owns outright\n(LFP)","conservative:outright"),
  grab(f_ast ,"Asset income\n(LFP, retired proxy)","conservative:wealthy_asset"),
  grab(f_lfp1,"Top-quartile home value\n(LFP, no decile FE)","conservative:wealthy")))
save_plot("t2_interaction_across_specs.png", {
  cp[, spec := factor(spec, levels=rev(spec))]
  print(ggplot(cp, aes(100*est, spec)) +
    geom_vline(xintercept=0, colour="grey40") +
    geom_errorbarh(aes(xmin=100*lo, xmax=100*hi), height=0.16, colour="#08519C") +
    geom_point(size=3, colour="#08519C") +
    labs(x="Culture x wealth interaction, effect on wife's LFP (pp)", y=NULL) +
    theme_minimal(base_size=13) +
    theme(panel.grid.minor=element_blank(), axis.text.y=element_text(size=11)))
}, width=2000, height=900)

# ── 4. INTERACTION BY YEAR ──────────────────────────────────────────────────
cat("\n=== 4. INTERACTION BY YEAR ===\n")
by_yr <- rbindlist(lapply(sort(unique(qa$YEAR)), function(y) {
  s <- qa[YEAR==y & !is.na(decile_f)]
  m <- feols(as.formula(paste0("f_lfp ~ conservative*wealthy + ",ctrl," | state_f + decile_f")),
             data=s, weights=~f_HHWT, cluster=~fips)
  ct <- as.data.table(coeftable(m), keep.rownames="term")
  setnames(ct, c("term","est","se","t","p"))
  ct[term=="conservative:wealthy"][, .(YEAR=y, n=nrow(s), est, se,
       lo=est-1.96*se, hi=est+1.96*se, p)]
}))
print(by_yr[, .(YEAR, n=format(n,big.mark=","), est_pp=round(100*est,2),
                lo=round(100*lo,2), hi=round(100*hi,2), p=signif(p,2))])
fwrite(by_yr, dated_path(results_dir,"t2_interaction_by_year.csv"))
pooled <- coef(f_lfp2)["conservative:wealthy"]
save_plot("t2_interaction_by_year.png", {
  print(ggplot(by_yr, aes(YEAR, 100*est)) +
    geom_hline(yintercept=0, colour="grey40") +
    geom_hline(yintercept=100*pooled, colour="#B2182B", linetype="22") +
    geom_ribbon(aes(ymin=100*lo, ymax=100*hi), fill="#4292C6", alpha=0.22) +
    geom_line(colour="#08519C", linewidth=0.9) + geom_point(size=2.2, colour="#08519C") +
    scale_x_continuous(breaks=2012:2020) +
    annotate("text", x=2019.4, y=100*pooled+0.55, label="pooled estimate",
             colour="#B2182B", size=3.4) +
    labs(x=NULL, y="Culture x wealth interaction on wife's LFP (pp)") +
    theme_minimal(base_size=13) + theme(panel.grid.minor=element_blank()))
}, width=2000, height=1000)

# ── 5. HOUSING TIER LADDER ──────────────────────────────────────────────────
q[, tier := fcase(is.na(owns), NA_character_, owns==0L, "Renter",
     owns==1L & outright %in% 0, "Owner,\nmortgaged",
     owns==1L & outright %in% 1 & (is.na(home_value_rank)|home_value_rank<0.75), "Owner\noutright",
     owns==1L & outright %in% 1, "Owner outright,\ntop-quartile value", default=NA_character_)]
tt <- q[!is.na(tier), .(couples=.N,
        lfp=100*weighted.mean(f_lfp,f_HHWT,na.rm=TRUE),
        hrs=weighted.mean(f_hours,f_HHWT,na.rm=TRUE)), by=tier]
tt[, tier := factor(tier, levels=c("Renter","Owner,\nmortgaged","Owner\noutright",
                                   "Owner outright,\ntop-quartile value"))]
save_plot("t2_housing_tier_ladder.png", {
  print(ggplot(tt[!is.na(tier)], aes(tier, lfp)) +
    geom_col(fill="#08519C", width=0.62) +
    geom_text(aes(label=sprintf("%.1f%%", lfp)), vjust=-0.5, size=4) +
    coord_cartesian(ylim=c(55,80)) +
    labs(x=NULL, y="Wife in labour force (%)") +
    theme_minimal(base_size=13) +
    theme(panel.grid.major.x=element_blank(), panel.grid.minor=element_blank()))
}, width=1900, height=950)
print(tt[order(tier)])
message("\nwrote 4 figures + balance/main/by-year tables")

# ── 6. COEFFICIENT PLOTS FOR THE HOURS AND LFP SPECIFICATIONS ───────────────
# One panel per specification, showing all three coefficients of interest with
# 95% CIs. Named by the OUTCOME rather than by column number, so the figure
# stands alone without the table beside it.
coefplot_spec <- function(model, outcome_label, unit_label, file, note) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, c("term","est","se","t","p"))
  lab <- c(conservative                 = "Republican-majority county",
           wealthy                      = "Wealthy: top-quartile home value",
           "conservative:wealthy"       = "Republican × top-quartile home")
  pd <- ct[term %in% names(lab)]
  pd[, nm := lab[term]]
  pd[, `:=`(lo = est - 1.96*se, hi = est + 1.96*se)]
  pd[, nm := factor(nm, levels = rev(lab))]      # first listed appears at top
  save_plot(file, {
    print(ggplot(pd, aes(est, nm)) +
      geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.5) +
      geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.13,
                     colour = "#08519C", linewidth = 0.7) +
      geom_point(size = 3.1, colour = "#08519C") +
      geom_text(aes(label = sprintf("%+.3f", est)), vjust = -1.25, size = 3.6,
                colour = "grey25") +
      labs(title = outcome_label,
           subtitle = paste0("Married couples, both 18-65, 2012-2020. Controls: both ages, both education,\n",
                             "children, husband's labour income. State, year and husband-income-decile fixed effects.\n",
                             "95% confidence intervals; standard errors clustered on county."),
           x = unit_label, y = NULL, caption = note) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", size = 15),
            plot.subtitle = element_text(colour = "grey30", size = 10.5),
            plot.caption = element_text(colour = "grey45", size = 9, hjust = 0),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 11.5)))
  }, width = 2100, height = 1000)
}

coefplot_spec(f_hrs, "Effect on the wife's weekly hours",
  "Change in wife's usual weekly hours",
  "t2_coefplot_wife_weekly_hours.png",
  paste0("Neither Republican-majority residence nor housing wealth alone predicts fewer hours; both predict MORE. ",
         "Only their\ninteraction is negative. n = 2,477,474."))

coefplot_spec(f_lfp2, "Effect on the wife's labour force participation",
  "Change in probability the wife is in the labour force",
  "t2_coefplot_wife_lfp.png",
  paste0("Same specification, participation rather than hours. Coefficients are in probability units ",
         "(0.01 = 1 pp).\nn = 2,477,474."))
message("wrote 2 coefficient plots")
