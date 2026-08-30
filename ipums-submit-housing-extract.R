# =============================================================================
# Supplementary IPUMS extract: HOUSING WEALTH for T2
#
# WHY: T2's wealth axis currently rests on INCINVST (reported interest,
# dividends, rent). That is a weak proxy and getting weaker -- positive for
# 23.9% of adults in 1980 but only 8-9% by 2022, which is reporting decay plus
# the migration of assets into tax-deferred accounts that pay no RECEIVED
# dividend. It reads asset-holding with error and understates it.
#
# Home equity is the median US household's dominant asset and is reported for
# owner-occupiers (~65% of households), so VALUEH + OWNERSHP + MORTGAGE gives a
# far better-populated wealth axis, plus a genuine wealth LEVEL rather than a
# binary "reports any asset income".
#
# SCOPE: 2012-2020 only -- the window where county presidential vote margins
# exist, which is what bounds T2. Deliberately narrow: the machine had ~19 GB
# free when this was written, so this requests three household variables rather
# than a full re-pull.
#
# MERGE KEY: YEAR + SAMPLE + SERIAL (household identifiers), against
# data/interim/ipums_bkp.sqlite.
# =============================================================================

library(ipumsr)
source("R/paths.R")

api_key <- Sys.getenv("IPUMS_API_KEY")
if (!nzchar(api_key)) {
  rl <- tryCatch(readLines(".Renviron", warn = FALSE), error = function(e) character())
  hit <- grep("^IPUMS_API_KEY=", rl, value = TRUE)
  if (length(hit)) api_key <- sub("^IPUMS_API_KEY=", "", hit[1])
}
if (!nzchar(api_key)) stop("No IPUMS_API_KEY found in the environment or .Renviron")
set_ipums_api_key(api_key)

housing_samples <- paste0("us", 2012:2020, "a")

ext <- define_extract_micro(
  collection  = "usa",
  description = "Housing wealth 2012-2020 for T2 culture x wealth quadrant",
  samples     = housing_samples,
  variables   = c("VALUEH",     # house value (owner-occupied); topcoded
                  "OWNERSHP",   # owned vs rented
                  "MORTGAGE")   # mortgage status -> equity vs gross value
)

submitted <- submit_extract(ext)
cat("\nSubmitted IPUMS extract:", submitted$collection, submitted$number, "\n")
cat("Samples:", paste(housing_samples, collapse = " "), "\n")
cat("\nTo download once ready, run ipums-build-housing-merge.R\n")
saveRDS(submitted$number, file.path(data_path("interim"), "housing_extract_number.rds"))
