library(ipumsr)
library(DBI)
library(RSQLite)
library(data.table)

source("R/paths.R")
source("functions.R")

# =========================================================
# Build the BKP replication database from IPUMS extract v4
#
# Downloads IPUMS USA extract #4 and loads it into a SQLite database.
#
# WHY A SEPARATE DATABASE: this writes to ipums_bkp.sqlite, NOT the existing
# 42GB ipums_data.sqlite. The old database stays untouched so every existing
# script (ipums-county-household-analysis.R, ipums-rdd-breadwinner-norm.R,
# ipums-married-household-suite.R, the frontier merge) keeps working unchanged
# while the BKP track moves to the new data. Once the new database is
# validated, the old one is redundant and can be deleted to reclaim ~39GB —
# but that is a manual decision, not something this script does.
#
# EXTRACT v4 CONTENTS (no overlapping samples — every person appears once):
#   - Decennial, highest-quality sample available per year:
#       1970: Form 1 State + Form 2 State (us1970a + us1970b). These are two
#             different questionnaire forms, so they do NOT overlap and combine
#             to ~2%. (The Metro/Neighborhood variants are the SAME records
#             recoded to different geography — including them would duplicate.)
#       1980, 1990, 2000: the 5% samples, the largest IPUMS offers.
#     These are the only decennials with income data: the 2010 and 2020
#     censuses were short-form only (no income, employment, or education), so
#     the ACS carries the series from 2001 on.
#   - ACS 1-year 2001-2024, every year.
#   - Deliberately EXCLUDES the ACS 3-year and 5-year products. BKP's Figure 1
#     used the 2008-2010 ACS 3-year aggregate, but that file contains the same
#     respondents as the 2008/2009/2010 1-year files, so including both would
#     double-count. We stack the 1-year files instead (documented deviation —
#     BKP's use of the 3-year file reflected the data vintage available to
#     them, not a property we need to reproduce).
#
# KEY VARIABLES ADDED vs. the old extract:
#   INCBUS / INCFARM (pre-2000) and INCBUS00 (2000+) -> lets us build BKP's
#     actual "labor income" = wage + self-employment, instead of wage-only.
#   HISPAN -> lets us match BKP's white/Black/Hispanic marriage-market split.
#   MARST, MARRNO, YRMARR, DIVINYR -> marriage timing; YRMARR in particular
#     supports BKP's "relative income at marriage" controls (Table 2 cols 11-12).
#   INCRETIR, plus allocation flags on the income variables (for the
#     Murray-Close & Heggeness misreporting diagnostic).
#   YNGCH, ELDCH, GQ, METRO, MULTGEN, PERWT -> NOT needed by the BKP scripts,
#     but required by the existing pipeline (ipums-county-household-analysis.R
#     reads YNGCH for its child-status LFPR panel and filters on GQ). They are
#     included so this database is a COMPLETE replacement for the old one and
#     the old 42GB file can be retired without breaking anything.
#
# DISK: the download is a few GB and the resulting database several more.
# The script refuses to start if free space looks insufficient.
# =========================================================

collection    <- "usa"
extract_num   <- 4L
chunk_size    <- 100000L
min_free_gb   <- 12      # refuse to start below this

interim_dir  <- data_path("interim")
download_dir <- file.path(interim_dir, "ipums_extract_v4")
db_path      <- file.path(interim_dir, "ipums_bkp.sqlite")
ensure_dir(download_dir)

# ── 0) Preflight: disk space ──────────────────────────────────────────────
free_gb <- tryCatch({
  df <- system2("df", c("-g", shQuote(interim_dir)), stdout = TRUE)
  as.numeric(strsplit(trimws(df[2]), "\\s+")[[1]][4])
}, error = function(e) NA_real_)

message("Free space on data volume: ",
        if (is.na(free_gb)) "unknown" else paste0(free_gb, " GB"))
if (!is.na(free_gb) && free_gb < min_free_gb) {
  stop("Only ", free_gb, " GB free; need ~", min_free_gb, " GB for the download ",
       "plus database build. Free up space first (the old 39GB ",
       "data/interim/ipums_data.sqlite is the biggest single candidate, but ",
       "only delete it AFTER this new database is validated).")
}

# ── 1) API key ─────────────────────────────────────────────────────────────
# Read from .Renviron (gitignored). Never hard-code the key in this script.
api_key <- Sys.getenv("IPUMS_API_KEY")
if (!nzchar(api_key)) {
  stop("IPUMS_API_KEY not set. Add it to .Renviron (which is gitignored) as:\n",
       "  IPUMS_API_KEY=your_key_here\n",
       "then restart R so it is picked up.")
}
set_ipums_api_key(api_key)

# ── 2) Wait for the extract, then download ────────────────────────────────
message("Checking IPUMS extract ", collection, ":", extract_num, " ...")
info <- get_extract_info(c(collection, extract_num))
message("  status: ", info$status)

if (!identical(info$status, "completed")) {
  message("  Extract not ready; waiting (checks every 60s, up to 2 hours) ...")
  info <- wait_for_extract(c(collection, extract_num),
                           initial_delay_seconds = 30,
                           max_delay_seconds     = 60,
                           timeout_seconds       = 7200)
}
if (!identical(info$status, "completed")) {
  stop("Extract did not complete (status: ", info$status, ").")
}

# Reuse an existing download ONLY if the gzip is intact. A partial .dat.gz is
# the dangerous case: it looks like a valid file to list.files(), and loading it
# would silently produce a database missing an arbitrary tail of the data.
# (This happened on the first attempt: the IPUMS download died on an HTTP/2
# PROTOCOL_ERROR at 2.66GB, leaving a truncated file behind.)
dat_files <- list.files(download_dir, pattern = "\\.dat\\.gz$", full.names = TRUE)
xml_files <- list.files(download_dir, pattern = "\\.xml$", full.names = TRUE)

gz_is_intact <- function(path) {
  isTRUE(tryCatch(
    system2("gzip", c("-t", shQuote(path)), stdout = FALSE, stderr = FALSE) == 0L,
    error = function(e) FALSE
  ))
}

have_valid_download <- length(dat_files) >= 1 && length(xml_files) >= 1 &&
                       gz_is_intact(dat_files[1])

if (have_valid_download) {
  message("  Extract files already present and gzip verified — skipping download.")
} else {
  if (length(dat_files) >= 1) {
    message("  Found a .dat.gz that fails its gzip integrity check (truncated ",
            "download). Resume it, or delete it and re-run:")
    message("    ", dat_files[1])
    stop("Refusing to load a truncated extract file.")
  }
  message("  Downloading extract to ", download_dir, " ...")
  download_extract(info, download_dir = download_dir, overwrite = FALSE)
  dat_files <- list.files(download_dir, pattern = "\\.dat\\.gz$", full.names = TRUE)
  if (length(dat_files) < 1 || !gz_is_intact(dat_files[1])) {
    stop("Download completed but the gzip is not intact — retry (the IPUMS ",
         "endpoint can drop HTTP/2 connections on large files; resuming with ",
         "`curl --http1.1 -C -` is reliable).")
  }
  message("  Download verified.")
}

ddi_file <- list.files(download_dir, pattern = "\\.xml$", full.names = TRUE)[1]
if (is.na(ddi_file)) stop("No DDI (.xml) file found in ", download_dir)
message("  DDI: ", basename(ddi_file))

# ── 3) Load into SQLite, chunked ──────────────────────────────────────────
# Chunked so memory stays bounded regardless of extract size — the full
# extract is far too large to hold in R at once.
if (file.exists(db_path)) {
  stop("Database already exists: ", db_path, "\n",
       "Delete it first if you intend to rebuild from scratch.")
}

ddi <- read_ipums_ddi(ddi_file)
con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "PRAGMA journal_mode = OFF")   # no rollback journal: faster, less disk
dbExecute(con, "PRAGMA synchronous = OFF")

n_written <- 0L
chunk_callback <- function(chunk_df, pos) {
  chunk_df <- as.data.frame(chunk_df)
  # Strip IPUMS labelled-vector attributes; store plain values for SQLite.
  chunk_df[] <- lapply(chunk_df, function(x) {
    if (inherits(x, "haven_labelled")) as.numeric(x) else x
  })
  dbWriteTable(con, "ipums_table", chunk_df, append = TRUE, row.names = FALSE)
  n_written <<- n_written + nrow(chunk_df)
  if (pos %% 2000000 < chunk_size) message("    ... ", format(n_written, big.mark = ","), " rows")
  NULL
}

message("Loading into ", db_path, " (chunked; this takes a while) ...")
# var_attrs = NULL: don't attach IPUMS value labels. We store plain numeric
# codes in SQLite, so building labelled vectors would only cost time/memory.
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(chunk_callback),
                          chunk_size = chunk_size, verbose = FALSE,
                          var_attrs = NULL)
message("  Wrote ", format(n_written, big.mark = ","), " person records.")

# ── 4) Index ──────────────────────────────────────────────────────────────
# Only this one index. It makes the per-year, per-sex age-range pulls in the
# BKP scripts index-only scans.
#
# DO NOT add an index on the spouse-join keys expecting it to help: with an
# age index present, SQLite's planner will use it for BOTH sides of a spouse
# self-join and nested-loop the match (measured: hours per year). The BKP
# scripts deliberately pull each side separately and join in R instead.
message("Creating index ...")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ipums_age_sex ON ipums_table (YEAR, AGE, SEX)")

# ── 5) Validate ───────────────────────────────────────────────────────────
message("\nRecords by year:")
yr <- setDT(dbGetQuery(con, "SELECT YEAR, COUNT(*) AS n FROM ipums_table GROUP BY YEAR ORDER BY YEAR"))
print(yr)

message("\nChecking the new income variables are populated:")
for (v in c("INCWAGE", "INCBUS", "INCFARM", "INCBUS00", "HISPAN", "MARST")) {
  ok <- tryCatch({
    q <- dbGetQuery(con, paste0(
      "SELECT COUNT(*) AS n_nonmissing FROM ipums_table WHERE ", v, " IS NOT NULL"))
    paste0(format(q$n_nonmissing, big.mark = ","), " non-null")
  }, error = function(e) paste0("COLUMN MISSING (", conditionMessage(e), ")"))
  message("  ", formatC(v, width = 10), ": ", ok)
}

# Is the 1970 Form 2 sample actually usable, or does it lack income variables?
# The analysis defaults to Form 1 only (see pull_person_side()); this reports
# whether pooling would be safe, so that stays an evidence-based choice rather
# than an assumption. If Form 2 shows income coverage comparable to Form 1,
# pooling is available as a one-line change (plus halving 1970 weights).
message("\n1970 form comparison — income coverage by sample:")
print(dbGetQuery(con, "
  SELECT SAMPLE,
         COUNT(*)                                                   AS n_records,
         SUM(CASE WHEN INCWAGE IS NOT NULL THEN 1 ELSE 0 END)       AS incwage_nonnull,
         SUM(CASE WHEN INCBUS  IS NOT NULL THEN 1 ELSE 0 END)       AS incbus_nonnull,
         SUM(CASE WHEN INCFARM IS NOT NULL THEN 1 ELSE 0 END)       AS incfarm_nonnull,
         SUM(CASE WHEN EDUC    IS NOT NULL THEN 1 ELSE 0 END)       AS educ_nonnull
  FROM ipums_table WHERE YEAR = 1970 GROUP BY SAMPLE ORDER BY SAMPLE"))
message("  (Analysis uses Form 1 = SAMPLE 197001. If Form 2 coverage is")
message("   comparable, pooling to ~2% is available — see pull_person_side().)")

message("\nSanity check — 2000 decennial present? (was missing from the old extract)")
print(dbGetQuery(con, "SELECT COUNT(*) AS n_2000 FROM ipums_table WHERE YEAR = 2000"))
message("Sanity check — 2024 present? (extends the panel past the old 2023 end)")
print(dbGetQuery(con, "SELECT COUNT(*) AS n_2024 FROM ipums_table WHERE YEAR = 2024"))

message("\nDatabase built: ", db_path)
message("Size: ", round(file.size(db_path) / 1024^3, 2), " GB")
message("\nNext: point the BKP scripts at this database and flip the income")
message("measure to wage + self-employment (see INCOME_MEASURE_SWAP).")
