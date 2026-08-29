# script for repeated functions

# Return YYYY-MM-DD for timestamped outputs.
today_tag <- function() {
  format(Sys.Date(), "%Y-%m-%d")
}

# Add date prefix unless it is already present.
with_date_prefix <- function(filename) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}_", filename)) {
    return(filename)
  }
  paste0(today_tag(), "_", filename)
}

# Save plots directly to data/graphs with consistent image settings.
save_plot <- function(filename, expr, width = 1800, height = 1200, res = 180, date_prefix = TRUE) {
  ensure_dir(graphs_dir())
  out_name <- if (date_prefix) with_date_prefix(filename) else filename
  out_file <- file.path(graphs_dir(), out_name)
  png(filename = out_file, width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  eval.parent(substitute(expr))
  invisible(out_file)
}

# Build a date-prefixed path in a directory and ensure parent exists.
dated_path <- function(dir_path, filename) {
  ensure_dir(dir_path)
  file.path(dir_path, with_date_prefix(filename))
}

# Zero-pad a county FIPS code to the canonical 5-character form.
#
# WHY THIS EXISTS: county FIPS codes carry meaningful leading zeros ("06037" =
# Los Angeles). data.table::fread types such a column as INTEGER, silently
# dropping the zero, while sprintf("%02d%03d", ...) elsewhere produces the
# padded string — so a merge between the two matches nothing for any state
# numbered 01-09, California included. That failure is silent: the join
# succeeds, the rows just never match.
#
# Measured cost when this went wrong: the spouse-pair political merge rate fell
# from 47.7% to 36.5% within the vote window, with all of California dropping
# out and no error raised anywhere.
#
# NOTE: writing the CSV with quote = TRUE does NOT solve this. fread() strips
# the quotes and re-types the column as integer anyway ("06037" -> 6037,
# verified). There is no write-side fix — every READER must call pad_fips() or
# pass colClasses = c(fips = "character").
#
# Naive sprintf() is also NA-unsafe: sprintf("%05d", NA_integer_) yields the
# literal "000NA", a string that looks like a valid code and defeats is.na()
# checks downstream. This helper preserves NA as NA.
pad_fips <- function(x) {
  n   <- suppressWarnings(as.integer(as.character(x)))
  out <- rep(NA_character_, length(n))
  ok  <- !is.na(n)
  out[ok] <- sprintf("%05d", n[ok])
  out
}

# CPI-U annual average, 1982-84 = 100 (BLS series CUUR0000SA0).
#
# Used to put dollar-denominated REGRESSORS on a common real basis when pooling
# across decades. Pooling 1970-2024 nominal dollars misspecifies any level or
# log control: $10,000 of husband's income means something entirely different in
# 1970 than in 2024, and year fixed effects shift the intercept without
# rescaling those slopes.
#
# NOTE ON WHAT NEEDS DEFLATING: ratios and probabilities are already scale-free.
# BKP's incomeGap = (realized - potential)/potential and PrWifeEarnsMore are both
# invariant to deflation; only the dollar controls (potential-income vigintiles,
# log husband's income) are affected. Measured impact on the Table 3 coefficient:
# about 10% (-0.088 -> -0.078), i.e. real but second-order next to functional form.
cpi_u_annual <- function() {
  data.frame(
    YEAR = c(1970, 1980, 1990, 2000:2024),
    cpi  = c(38.8, 82.4, 130.7,
             172.2, 177.1, 179.9, 184.0, 188.9, 195.3, 201.6, 207.342,
             215.303, 214.537, 218.056, 224.939, 229.594, 232.957, 236.736,
             237.017, 240.007, 245.120, 251.107, 255.657, 258.811, 270.970,
             292.655, 304.702, 313.689)
  )
}

# Convert nominal dollars to constant dollars of `base_year`.
deflate_to <- function(x, year, base_year = 2010) {
  cpi <- cpi_u_annual()
  idx <- cpi$cpi[match(as.integer(year), cpi$YEAR)]
  base <- cpi$cpi[cpi$YEAR == base_year]
  if (length(base) != 1L) stop("base_year not in CPI table: ", base_year)
  x * base / idx
}
