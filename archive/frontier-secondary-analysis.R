# =============================================================================
# FRONTIER — SECONDARY ANALYSIS (Bazzi, Fiszbein & Gebresilasse)
#
# STATUS: SECONDARY / ARCHIVED. Not part of the main T1-T2-T3 pipeline and not
# run by any other script. The frontier line was dropped from the project on
# 2026-08-30; this file preserves the analysis in one place so the work is not
# lost and can be re-run or cited if it is ever revived. Its outputs belong in
# data/graphs/archive/, not alongside live results.
#
# WHY IT WAS DROPPED: total frontier experience is a historical settlement
# measure used as an alternative culture proxy. It duplicates what the county
# vote margin already does in T2, adds a second attenuating proxy on top of an
# attenuating one, and the T2 political interaction turned out null under state
# fixed effects and county clustering — so the frontier stratification had
# nothing left to amplify. Keeping it in the main scripts implied it was a live
# part of the argument, which it is not.
#
# WHAT IT SHOWED, for the record: the below/above 0.5 cliff ratio was ordered
# Rep x frontier (1.576) > Dem x frontier (1.462) > Rep x non-frontier (1.489)
# > Dem x non-frontier (1.420), i.e. the Rep-Dem gap was 0.114 in frontier
# counties against 0.069 elsewhere — 65% larger. NOTE: an earlier run with a
# broken FIPS merge reported 72%; that number is wrong and should not be cited.
#
# CONSOLIDATED FROM (removed from those scripts on 2026-08-30):
#   ipums-rdd-breadwinner-norm.R   Section 9  — decile graphs x frontier
#   ipums-bkp-augmented-tests.R    Section C  — cliff ratio by political x frontier
#   lfpr-groupings.R               Sections 6e, 6f  — LFPR x frontier  (NOT carried
#     over; those were exploratory LFPR scatters whose outputs are archived. Recover
#     from git history if needed.)
#   ipums-married-household-suite.R Section 12e — work share x frontier  (same)
#
# The lookup itself is built by bazzi-frontier-merge.R, which is unchanged.
# =============================================================================

library(data.table)
library(ggplot2)

source(here::here("_setup.R"))

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

pol_colors <- c("Democratic-majority" = "#2166AC", "Republican-majority" = "#B2182B")

frontier_lu <- fread(
  file.path(panel_dir, "bazzi_frontier_indicators.csv"),
  select = c("fips", "is_frontier")
)
# pad_fips(): fread types FIPS as integer and drops leading zeros. Without
# padding BOTH sides, every state numbered 01-09 silently fails to merge --
# this is the bug that produced the discredited 72% figure.
frontier_lu[, fips := pad_fips(fips)]

# ── Rebuild the decile panel (was Section 9 of the RDD script) ───────────────
decile_cols <- c("YEAR", "HHWT", "fips",
                 "female_weekly_hours", "male_weekly_hours",
                 "female_empstat", "vote_margin",
                 "hhincome_nominal",
                 "female_income_no_transfers", "male_income_no_transfers")

pairs_file_grp <- file.path(panel_dir,
  "ipums_married_oppositesex_spouse_pairs_with_groups.csv")

avail_cols <- names(fread(pairs_file_grp, nrows = 0))
load_cols  <- intersect(decile_cols, avail_cols)

message("  Loading pairs-with-groups for decile analysis ...")
dec_dt <- fread(pairs_file_grp, select = load_cols, showProgress = FALSE)
dec_dt <- dec_dt[YEAR %in% 2010:2020 & !is.na(vote_margin)]

# Income deciles within year from hhincome_nominal
dec_dt[, income_decile := cut(
  hhincome_nominal,
  breaks  = quantile(hhincome_nominal, probs = seq(0, 1, 0.1), na.rm = TRUE),
  labels  = 1:10,
  include.lowest = TRUE
), by = YEAR]
dec_dt[, income_decile := as.integer(as.character(income_decile))]

# Political group (±5pp buffer, consistent with other scripts)
dec_dt[, political := fcase(
  vote_margin >  0.05, "Democratic-majority",
  vote_margin < -0.05, "Republican-majority",
  default = NA_character_
)]

# Merge frontier
dec_dt[, fips := pad_fips(fips)]
dec_dt <- merge(dec_dt, frontier_lu, by = "fips", all.x = TRUE)
dec_dt[, frontier_label := fifelse(is_frontier == 1,
                                   "Frontier counties",
                                   "Non-frontier counties")]

# ── 9a) Hours by income decile × political × frontier ─────────────────────────

hours_long <- dec_dt[
  !is.na(income_decile) & !is.na(political) & !is.na(is_frontier),
  .(
    wife_hours    = weighted.mean(female_weekly_hours, HHWT, na.rm = TRUE),
    husband_hours = weighted.mean(male_weekly_hours,   HHWT, na.rm = TRUE)
  ),
  by = .(frontier_label, political, income_decile)
]

hours_melt <- melt(hours_long,
  id.vars       = c("frontier_label", "political", "income_decile"),
  measure.vars  = c("wife_hours", "husband_hours"),
  variable.name = "spouse",
  value.name    = "hours"
)
hours_melt[, spouse_label := fifelse(spouse == "wife_hours", "Wife", "Husband")]
hours_melt[, linetype_val := fifelse(spouse == "wife_hours", "solid", "dashed")]

pol_colors <- c("Democratic-majority" = "#4575b4",
                "Republican-majority"  = "#d73027")

p_hours_front <- ggplot(
  hours_melt,
  aes(x = income_decile, y = hours,
      color    = political,
      linetype = spouse_label,
      group    = interaction(political, spouse_label))
) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 1.8) +
  facet_wrap(~frontier_label) +
  scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10)) +
  scale_color_manual(values = pol_colors, name = NULL) +
  scale_linetype_manual(values = c("Wife" = "solid", "Husband" = "dashed"),
                        name = NULL) +
  labs(
    title    = "Weekly hours by income decile: husband & wife × Dem/Rep × frontier",
    subtitle = "Married opposite-sex pairs, IPUMS 2010-2020; frontier = Bazzi et al. (2025)",
    x        = "Household income decile (within year)",
    y        = "Mean weekly hours"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())
save_plot("hours_by_income_decile_husband_wife_dem_vs_rep_frontier.png",
          { print(p_hours_front) }, width = 2400, height = 1200)

# ── Wife's earned share by decile x political x frontier ─────────────────────

share_dt <- dec_dt[
  female_income_no_transfers > 0 & male_income_no_transfers > 0 &
  !is.na(income_decile) & !is.na(political)
]
share_dt[, z_earned_dec := female_income_no_transfers /
           (female_income_no_transfers + male_income_no_transfers)]

share_agg <- share_dt[, .(
  wife_earned_share = weighted.mean(z_earned_dec, HHWT, na.rm = TRUE)
), by = .(frontier_label, political, income_decile)]

p_share_front <- ggplot(
  share_agg,
  aes(x = income_decile, y = wife_earned_share * 100,
      color = political, group = political)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  facet_wrap(~frontier_label) +
  scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10)) +
  scale_color_manual(values = pol_colors, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Wife's earned income share by decile: Dem vs Rep × frontier status",
    subtitle = "Couples with both spouses earning > 0; IPUMS 2010-2020; frontier = Bazzi et al. (2025)",
    x        = "Household income decile (within year)",
    y        = "Wife's share of couple earned income (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())
save_plot("share_vs_hh_income_decile_dem_vs_rep_frontier.png",
          { print(p_share_front) }, width = 2400, height = 1200)

message("Frontier secondary analysis complete. Outputs are SECONDARY — file them under data/graphs/archive/.")
