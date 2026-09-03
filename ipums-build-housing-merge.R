# =============================================================================
# Build the housing-wealth lookup (IPUMS extract usa:6) for T2
#
# Produces ONE ROW PER HOUSEHOLD keyed on YEAR + SAMPLE + SERIAL, so it can be
# merged onto any couple-level analysis without changing that analysis's sample.
#
# WEALTH MEASURES BUILT HERE
#   owns          1 if owner-occupied (OWNERSHP == 1)
#   owns_outright 1 if owned free and clear (MORTGAGE == 1) -- the strongest
#                 single wealth signal in the ACS: no debt against the asset
#   home_value    VALUEH, sentinels stripped. Gross house value, NOT equity --
#                 the ACS carries no mortgage BALANCE, so true equity is not
#                 recoverable. Renters are NA, not zero.
#
# WHY THIS BEATS THE INCINVST AXIS IT SUPPLEMENTS: home equity is the median US
# household's dominant asset, ownership is reported for ~65% of households
# (against 8-15% reporting any asset income), and VALUEH is a LEVEL rather than
# a binary "reports something".
# =============================================================================

library(ipumsr)
library(data.table)
source(here::here("_setup.R"))

# Builds the housing-wealth lookup for T2, one row per household.
# Input  : the downloaded usa:6 extract
# Output : data/processed/panel/ housing lookup keyed on YEAR + SAMPLE + SERIAL

api_key <- Sys.getenv("IPUMS_API_KEY")
if (!nzchar(api_key)) {
  rl  <- tryCatch(readLines(".Renviron", warn = FALSE), error = function(e) character())
  hit <- grep("^IPUMS_API_KEY=", rl, value = TRUE)
  if (length(hit)) api_key <- sub("^IPUMS_API_KEY=", "", hit[1])
}
set_ipums_api_key(api_key)

interim_dir  <- data_path("interim")
download_dir <- file.path(interim_dir, "ipums_extract_v6_housing")
ensure_dir(download_dir)

free_gb <- as.numeric(system(paste("df -g", shQuote(interim_dir),
                                   "| tail -1 | awk '{print $4}'"), intern = TRUE))
message("Free disk: ", free_gb, " GB")
if (is.finite(free_gb) && free_gb < 6) {
  stop("Only ", free_gb, " GB free; refusing to download. Free space first.")
}

info <- get_extract_info(c("usa", 6))
if (!identical(info$status, "completed")) stop("Extract usa:6 is not ready: ", info$status)

dat_files <- list.files(download_dir, pattern = "\\.dat\\.gz$", full.names = TRUE)
if (length(dat_files) == 0L) {
  message("Downloading extract usa:6 ...")
  download_extract(info, download_dir = download_dir, overwrite = FALSE)
}

ddi_file <- list.files(download_dir, pattern = "\\.xml$", full.names = TRUE)[1]
if (is.na(ddi_file)) stop("No DDI found in ", download_dir)

message("Reading housing records ...")
hh <- setDT(read_ipums_micro(read_ipums_ddi(ddi_file), verbose = FALSE))
message("  person records read: ", format(nrow(hh), big.mark = ","))

keep <- intersect(c("YEAR", "SAMPLE", "SERIAL", "VALUEH", "OWNERSHP", "MORTGAGE"),
                  names(hh))
hh <- hh[, ..keep]
hh <- unique(hh, by = c("YEAR", "SAMPLE", "SERIAL"))   # -> one row per household
message("  households after dedupe: ", format(nrow(hh), big.mark = ","))

# VALUEH sentinels: 9999999 = N/A, and 0 is used for "not applicable" (renters),
# not a house worth nothing. Both become NA so they cannot be averaged as zero.
hh[, home_value := as.numeric(VALUEH)]
hh[home_value >= 9999999 | home_value <= 0, home_value := NA_real_]

hh[, owns          := as.integer(OWNERSHP == 1L)]
hh[OWNERSHP == 0L, owns := NA_integer_]                # 0 = N/A (e.g. group qtrs)
hh[, owns_outright := as.integer(MORTGAGE == 1L)]      # owned free and clear
hh[is.na(MORTGAGE) | MORTGAGE == 0L, owns_outright := NA_integer_]
hh[owns %in% 0L, owns_outright := 0L]                  # renters are not outright owners

message("\nCoverage by year:")
print(hh[, .(households   = .N,
             pct_owns     = round(100 * mean(owns, na.rm = TRUE), 1),
             pct_outright = round(100 * mean(owns_outright, na.rm = TRUE), 1),
             pct_valueh   = round(100 * mean(!is.na(home_value)), 1),
             med_value    = round(median(home_value, na.rm = TRUE))),
          by = YEAR][order(YEAR)])

out <- file.path(data_path("processed", "panel"), "housing_wealth_by_household.csv")
fwrite(hh[, .(YEAR, SAMPLE, SERIAL, home_value, owns, owns_outright)], out)
message("\nwrote: ", out, "  (", format(nrow(hh), big.mark = ","), " households)")
