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
#
# bg = "white" is explicit: several themes leave the plot background
# transparent, which renders as black in some PDF viewers and slide software.
#
# also_pdf = TRUE writes a vector copy alongside the PNG, for LaTeX inclusion.
# The expression is re-evaluated on the second device rather than reusing the
# object, so the two files are drawn from identical code.
save_plot <- function(filename, expr, width = 1800, height = 1200, res = 180,
                      date_prefix = TRUE, also_pdf = FALSE) {
  ensure_dir(graphs_dir())
  out_name <- if (date_prefix) with_date_prefix(filename) else filename
  out_file <- file.path(graphs_dir(), out_name)
  code <- substitute(expr)
  env  <- parent.frame()

  png(filename = out_file, width = width, height = height, res = res, bg = "white")
  tryCatch(eval(code, env), finally = dev.off())

  if (also_pdf) {
    pdf(file.path(graphs_dir(), sub("\\.png$", ".pdf", out_name)),
        width = width / res, height = height / res, bg = "white")
    tryCatch(eval(code, env), finally = dev.off())
  }
  invisible(out_file)
}

# Read the most recently dated file matching `pattern` from a directory.
# Used by every figure script, so that redrawing a figure always picks up the
# current results rather than whichever file happens to sort first.
#
# Date-prefixed files are preferred over undated ones. This matters because
# results/ still holds undated files written before the date convention: digits
# sort before letters, so a plain sort() would place the undated (stale) file
# LAST and silently select it.
read_newest <- function(dir_path, pattern) {
  f <- list.files(dir_path, pattern, full.names = TRUE)
  if (!length(f)) stop("No file matching '", pattern, "' in ", dir_path)
  dated <- grepl("/[0-9]{4}-[0-9]{2}-[0-9]{2}_[^/]*$", f)
  f <- if (any(dated)) sort(f[dated]) else sort(f)
  data.table::fread(f[length(f)])
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

# Weeks worked last year, reconciling the two IPUMS encodings.
#
# WKSWORK1 (continuous, 0-52) is ABSENT for 2008-2018 and for 1970: the ACS
# asked a categorical question in those years. WKSWORK2 gives 6 bins instead.
# Building annual hours from WKSWORK1 alone silently yields ZERO hours for
# 2008-2018 — which is how the T2 hourly-wage analysis ended up running on
# 1980/1990/2000/2005-07/2019-24 only, without any error being raised.
#
# The bin -> weeks values below are EMPIRICAL means of WKSWORK1 within each
# WKSWORK2 bin, measured on the 37M overlap-year records where both variables
# exist (1980-2007, 2019-2024). They are NOT textbook midpoints, which are
# biased in the two bins that matter most: naive 51 vs true 51.86 for bin 6
# (67.4% of all workers) and naive 43.5 vs true 42.33 for bin 4.
#
# Measured cost of the imputation, on those overlap years:
#   mean absolute error   = 1.07 weeks
#   mean |error| in log hourly wage = 3.06%
# Accuracy is very uneven: bins 5-6 (71% of workers, 48+ weeks) have within-bin
# SD under 0.5 weeks, so full-year workers are near-exact. The error is
# concentrated in part-YEAR workers, who are disproportionately the women whose
# labour supply this project cares about — so treat 3% as an average, not a
# uniform bound, and flag interval-derived years in any output.
wkswork2_to_weeks <- function(bin) {
  map <- c(`1` = 7.35, `2` = 20.97, `3` = 32.98,
           `4` = 42.33, `5` = 48.21, `6` = 51.86)
  out <- unname(map[as.character(bin)])
  out[is.na(out)] <- NA_real_
  out
}

# Weeks worked, preferring the continuous measure and falling back to the
# interval reconstruction. Returns NA where neither is usable.
weeks_worked <- function(wkswork1, wkswork2) {
  w1 <- suppressWarnings(as.numeric(wkswork1))
  w1[!is.finite(w1) | w1 < 1 | w1 > 52] <- NA_real_
  w2 <- wkswork2_to_weeks(wkswork2)
  ifelse(!is.na(w1), w1, w2)
}

# TRUE where weeks had to be reconstructed from the interval variable, so that
# any result can be re-checked excluding those years.
weeks_is_imputed <- function(wkswork1, wkswork2) {
  w1 <- suppressWarnings(as.numeric(wkswork1))
  bad1 <- !is.finite(w1) | w1 < 1 | w1 > 52
  bad1 & !is.na(wkswork2_to_weeks(wkswork2))
}

# STATEICP -> state FIPS crosswalk. Lives here because both the RDD script and
# the T2 quadrant script need it to build county FIPS keys; keeping one copy
# avoids the two drifting apart.
stateicp_fips_xwalk <- function() {
  data.table::data.table(
    STATEICP   = c(1,2,3,4,5,6,11,12,13,14,21,22,23,24,25,
                   31,32,33,34,35,36,37,40,41,42,43,44,45,46,47,48,49,
                   51,52,53,54,56,61,62,63,64,65,66,67,68,71,72,73,81,82,98),
    state_fips = c(9,23,25,33,44,50,10,34,36,42,17,18,26,39,55,
                   19,20,27,29,31,38,46,51,1,5,12,13,22,28,37,45,48,
                   21,24,40,47,54,4,8,16,30,32,35,49,56,6,41,53,2,15,11)
  )
}

# County FIPS from IPUMS STATEICP + COUNTYICP. COUNTYICP carries a trailing
# digit that must be divided out before zero-padding to the 3-digit county part.
# Returns NA where the state is unmatched or the county is unidentified (0).
build_county_fips <- function(stateicp, countyicp) {
  st <- as.integer(stateicp)
  co <- as.integer(countyicp)
  xw <- stateicp_fips_xwalk()
  # match() rather than merge(): a join would risk silently reordering rows
  # relative to the caller's data, which is unrecoverable once assigned back.
  sf <- xw$state_fips[match(st, xw$STATEICP)]
  ok <- !is.na(sf) & !is.na(co) & co > 0
  out <- rep(NA_character_, length(st))
  out[ok] <- sprintf("%02d%03d", sf[ok], as.integer(floor(co[ok] / 10)))
  out
}
