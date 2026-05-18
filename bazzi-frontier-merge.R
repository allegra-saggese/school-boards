library(sf)
library(data.table)
library(ggplot2)

source("functions.R")
source("R/paths.R")

# =========================================================
# Bazzi et al. frontier indicator merge
#
# Source: Bazzi, Fiszbein & Gebresilasse — Frontier Culture
# replication GDB (FrontierCultureReplication/data/GIS/maps.gdb)
#
# Key variable: tfe890_500kNI_100_l6 — total frontier experience
# (number of census decades 1790-1890 the county had pop density
# < 6/sq mi, within 100km of the frontier line, non-incorporated).
# Range: 0-63. Counties with TFE=0 were never frontier (primarily
# original eastern seaboard colonies).
#
# Outputs:
#   data/processed/panel/bazzi_frontier_indicators.csv — county lookup
#   data/graphs/YYYY-MM-DD_frontier_map_tfe.png
#   data/graphs/YYYY-MM-DD_frontier_map_binary.png
#   data/graphs/YYYY-MM-DD_frontier_norm_gap_by_income_quintile.png
# =========================================================

today        <- format(Sys.Date(), "%Y-%m-%d")
gdb_path     <- "FrontierCultureReplication/data/GIS/maps.gdb"
panel_dir    <- data_path("processed", "panel")
graphs_dir   <- "data/graphs"
ensure_dir(graphs_dir)

# ── 1) Extract frontier indicators from GDB ────────────────────────────────

message("Reading TFE county layer from GDB ...")
tfe_layer <- st_read(gdb_path, layer = "US_county_2010_TFE_Popdens", quiet = TRUE)

# Drop geometry — we only need the attribute table for the merge
tfe_df <- as.data.table(tfe_layer)
st_geometry(tfe_layer) <- NULL

tfe_lookup <- tfe_df[, .(
  fips             = as.integer(fips),
  tfe              = tfe890_500kNI_100_l6,   # continuous TFE (0-63 decades)
  is_frontier      = as.integer(tfe890_500kNI_100_l6 > 0),
  yr_entered       = yrent500kNI_l6100,       # first year county was frontier
  samp_ew1890      = sampE_W1790E1890,        # Bazzi's main sample indicator
  statename        = statename,
  countyname       = countyname
)]

message(sprintf(
  "Frontier lookup: %d counties, %d frontier (%.1f%%), %d non-frontier",
  nrow(tfe_lookup),
  sum(tfe_lookup$is_frontier, na.rm = TRUE),
  100 * mean(tfe_lookup$is_frontier, na.rm = TRUE),
  sum(tfe_lookup$is_frontier == 0, na.rm = TRUE)
))

# Save lookup
out_lookup <- file.path(panel_dir, "bazzi_frontier_indicators.csv")
fwrite(tfe_lookup, out_lookup)
message("Saved: ", out_lookup)

# ── 2) Merge with ACS panel ────────────────────────────────────────────────

message("Loading ACS panel ...")
panel_files <- sort(list.files(panel_dir, pattern = "lfpr_panel_with_groups\\.csv$", full.names = TRUE))
panel_file  <- tail(panel_files, 1)
message("Using: ", basename(panel_file))

panel <- fread(panel_file)
panel[, fips := as.integer(fips)]
panel_merged <- merge(panel, tfe_lookup[, .(fips, tfe, is_frontier, yr_entered, samp_ew1890)],
                      by = "fips", all.x = TRUE)

message(sprintf(
  "Panel merge: %d rows total, %d (%.1f%%) matched to frontier lookup",
  nrow(panel_merged),
  sum(!is.na(panel_merged$is_frontier)),
  100 * mean(!is.na(panel_merged$is_frontier))
))

# ── 3) US county map — binary frontier status ─────────────────────────────

message("Building frontier maps ...")
county_sf <- st_read(gdb_path, layer = "US_county_2010", quiet = TRUE)
county_sf  <- merge(county_sf, tfe_lookup[, .(fips, tfe, is_frontier)],
                    by = "fips", all.x = TRUE)

# Exclude Alaska, Hawaii, Puerto Rico for contiguous US map
county_cont <- county_sf[!county_sf$STATEFP10 %in% c("02", "15", "72"), ]

p_binary <- ggplot(county_cont) +
  geom_sf(aes(fill = factor(is_frontier)), color = NA, linewidth = 0) +
  scale_fill_manual(
    values = c("0" = "#E8E8E8", "1" = "#4A6FA5"),
    labels = c("0" = "Non-frontier", "1" = "Frontier"),
    na.value = "white",
    name = NULL
  ) +
  labs(
    title    = "Frontier counties (Bazzi et al.)",
    subtitle = "County ever had pop. density < 6/sq mi within 100km of frontier line, 1790–1890",
    caption  = "Source: Bazzi, Fiszbein & Gebresilasse frontier replication data"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    legend.position = "bottom"
  )

ggsave(file.path(graphs_dir, paste0(today, "_frontier_map_binary.png")),
       p_binary, width = 10, height = 6.5, dpi = 150)

# ── 4) US county map — continuous TFE ─────────────────────────────────────

p_tfe <- ggplot(county_cont) +
  geom_sf(aes(fill = tfe), color = NA, linewidth = 0) +
  scale_fill_gradientn(
    colors   = c("#F5F5F5", "#B8CCE4", "#4A6FA5", "#1B3A6B"),
    na.value = "white",
    name     = "Frontier decades\n(TFE, 0–63)"
  ) +
  labs(
    title    = "Total frontier experience by county",
    subtitle = "Number of census decades (1790–1890) with pop. density < 6/sq mi",
    caption  = "Source: Bazzi, Fiszbein & Gebresilasse frontier replication data"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    legend.position = "right"
  )

ggsave(file.path(graphs_dir, paste0(today, "_frontier_map_tfe.png")),
       p_tfe, width = 10, height = 6.5, dpi = 150)

message("Saved frontier maps.")

# ── 5) Income-elastic norm gap: frontier vs non-frontier ──────────────────
#
# Replicate the core finding (female work share by HH income quintile x
# political group) but stratified by frontier status. Prediction: the
# income-elastic gap (Q5 > Q1-Q3) should be amplified in frontier counties.

message("Loading IPUMS pair data for frontier stratification ...")

pairs_file <- file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_groups.csv")
if (!file.exists(pairs_file)) {
  message("Pair file not found — skipping stratified graph. Run ipums-county-household-analysis.R first.")
  quit(save = "no")
}

cols_needed <- c("YEAR", "HHWT", "STATEICP", "COUNTYICP",
                 "female_empstat", "income_quintile",
                 "vote_margin", "fips")

pairs <- fread(pairs_file,
               select = intersect(cols_needed, fread(pairs_file, nrows = 0) |> names()),
               showProgress = FALSE)

# Check if fips is in pair file (added in Section 7 of ipums-county-household-analysis.R)
if (!"fips" %in% names(pairs)) {
  message("fips not in pair file — skipping stratified graph.")
  quit(save = "no")
}

pairs[, fips := as.integer(fips)]
pairs <- merge(pairs, tfe_lookup[, .(fips, is_frontier)], by = "fips", all.x = TRUE)

# Create political group from vote_margin (consistent with other scripts)
pairs[, political_group := fifelse(vote_margin > 0.05, "Dem",
                            fifelse(vote_margin < -0.05, "Rep", NA_character_))]

# Restrict to 2010-2020, both Dem/Rep, known frontier status
pairs_sub <- pairs[
  YEAR >= 2010 & YEAR <= 2020 &
  political_group %in% c("Dem", "Rep") &
  !is.na(is_frontier) &
  !is.na(income_quintile) &
  income_quintile %in% 1:5
]

# Female work share by quintile x political group x frontier status
gap_dt <- pairs_sub[, .(
  female_work_share = weighted.mean(female_empstat == 1, w = HHWT, na.rm = TRUE)
), by = .(income_quintile, political_group, is_frontier)]

gap_dt[, frontier_label := ifelse(is_frontier == 1, "Frontier", "Non-frontier")]
gap_dt[, group_label := paste0(political_group, " (", frontier_label, ")")]

palette <- c(
  "Dem (Frontier)"     = "#3B6FA5",
  "Dem (Non-frontier)" = "#9BB8D4",
  "Rep (Frontier)"     = "#C43131",
  "Rep (Non-frontier)" = "#E89090"
)

p_gap <- ggplot(gap_dt, aes(x = income_quintile, y = female_work_share,
                             color = group_label, group = group_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = palette, name = NULL) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Female work share by income quintile: political lean × frontier status",
    subtitle = "Married spouse pairs, IPUMS 2010–2020",
    x        = "HH income quintile (national)",
    y        = "Female spouse working (%)",
    caption  = "Frontier = ever had pop density < 6/sq mi within 100km of frontier, 1790–1890 (Bazzi et al.)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(graphs_dir, paste0(today, "_frontier_norm_gap_by_income_quintile.png")),
       p_gap, width = 9, height = 6, dpi = 150)

message("Saved: frontier_norm_gap_by_income_quintile.png")
message("Done.")
