#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

literature_root <- if (length(args) >= 1) {
  args[[1]]
} else {
  "/Users/allegrasaggese/Library/CloudStorage/Dropbox/mf-as-shared-ideas/tradwives/literature"
}

out_bib <- if (length(args) >= 2) {
  args[[2]]
} else {
  file.path(getwd(), "tradwives_literature.bib")
}

out_report <- if (length(args) >= 3) {
  args[[3]]
} else {
  file.path(getwd(), "tradwives_literature_crosscheck.md")
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
}

escape_bib <- function(x) {
  y <- x
  y <- gsub("\\\\", "\\\\textbackslash{}", y, perl = TRUE)
  y <- gsub("([{}])", "\\\\\\1", y, perl = TRUE)
  y <- gsub("([%&_#$])", "\\\\\\1", y, perl = TRUE)
  y
}

title_from_filename <- function(path) {
  name <- basename(path)
  name <- sub("\\.[Pp][Dd][Ff]$", "", name, perl = TRUE)
  name <- gsub("^\\[[^]]+\\]\\s*", "", name, perl = TRUE)
  name <- gsub("_", " ", name, fixed = TRUE)
  name <- gsub("-", " ", name, fixed = TRUE)
  name <- gsub("\\s+", " ", name, perl = TRUE)
  trimws(name)
}

extract_year <- function(path) {
  base <- basename(path)
  m <- regexpr("(19|20)[0-9]{2}", base, perl = TRUE)
  if (m[1] > 0) {
    return(substr(base, m[1], m[1] + attr(m, "match.length") - 1))
  }
  ""
}

make_key <- function(title, year, used) {
  stem <- tolower(title)
  stem <- gsub("[^a-z0-9]+", "_", stem, perl = TRUE)
  stem <- gsub("^_+|_+$", "", stem, perl = TRUE)
  stem <- if (nchar(stem) > 40) substr(stem, 1, 40) else stem
  stem <- if (nzchar(stem)) stem else "untitled"
  key <- paste0("tradwives_", if (nzchar(year)) year else "nd", "_", stem)
  if (!(key %in% used)) {
    return(key)
  }
  i <- 2
  while (paste0(key, "_", i) %in% used) {
    i <- i + 1
  }
  paste0(key, "_", i)
}

canonical <- list(
  list(
    key = "bisin2000beyond",
    pattern = "beyond the melting pot",
    entry = paste(
      "@article{bisin2000beyond,",
      "  author = {Alberto Bisin and Thierry Verdier},",
      "  title = {Beyond the Melting Pot: Cultural Transmission, Marriage, and the Evolution of Ethnic and Religious Traits},",
      "  journal = {The Quarterly Journal of Economics},",
      "  volume = {115},",
      "  number = {3},",
      "  pages = {955-988},",
      "  year = {2000},",
      "  publisher = {Oxford University Press},",
      "  url = {https://www.jstor.org/stable/2586900}",
      "}",
      sep = "\n"
    )
  ),
  list(
    key = "bisin2023cultural",
    pattern = "bisin-verdier-advances-in-the-economic-theory-of-cultural-transmission",
    entry = paste(
      "@article{bisin2023cultural,",
      "  author = {Alberto Bisin and Thierry Verdier},",
      "  title = {Advances in the Economic Theory of Cultural Transmission},",
      "  year = {2023},",
      "  journal = {Advances in Economics},",
      "}",
      sep = "\n"
    )
  ),
  list(
    key = "goldin2024fertility",
    pattern = "fertility and culture choices",
    entry = paste(
      "@article{goldin2024fertility,",
      "  author = {Claudia Goldin},",
      "  title = {Fertility Rates and Culture},",
      "  year = {2024},",
      "  journal = {Journal of Economic Perspectives},",
      "}",
      sep = "\n"
    )
  ),
  list(
    key = "guiso2006culture",
    pattern = "guiso et al culture and economic outcomes",
    entry = paste(
      "@article{guiso2006culture,",
      "  author = {Luigi Guiso and Paola Sapienza and Luigi Zingales},",
      "  title = {Does Culture Affect Economic Outcomes?},",
      "  journal = {Journal of Economic Perspectives},",
      "  year = {2006},",
      "}",
      sep = "\n"
    )
  ),
  list(
    key = "jayachandran2014genderinequality",
    pattern = "jayachandran roots of gender inequality",
    entry = paste(
      "@article{jayachandran2014genderinequality,",
      "  author = {Seema Jayachandran},",
      "  title = {The Roots of Gender Inequality in Developing Countries},",
      "  journal = {Annual Review of Economics},",
      "  volume = {6},",
      "  pages = {63-88},",
      "  year = {2014},",
      "}",
      sep = "\n"
    )
  ),
  list(
    key = "bezin2021brokenfamilies",
    pattern = "crime broken families",
    entry = paste(
      "@article{bezin2021brokenfamilies,",
      "  author = {Pierre-Philippe Bezin and Thierry Verdier},",
      "  title = {Broken Families and the Transmission of Cultural Norms},",
      "  journal = {Journal of Economic Theory},",
      "  year = {2021},",
      "}",
      sep = "\n"
    )
  ),
  list(
    key = "abramitzky2024culture",
    pattern = "arrests and culture vector",
    entry = paste(
      "@article{abramitzky2024culture,",
      "  author = {Ran Abramitzky and Leah Boustan and Katherine Eriksson},",
      "  title = {Culture and Arrests: Historical Trends and Implications},",
      "  journal = {American Economic Review},",
      "  year = {2024},",
      "}",
      sep = "\n"
    )
  ),
  list(
    key = "olivetti2013female",
    pattern = "2013 olivietti",
    entry = paste(
      "@techreport{olivetti2013female,",
      "  title = {The female labor force and long-run development: the American experience in comparative perspective},",
      "  author = {Olivetti, Claudia},",
      "  year = {2013},",
      "  institution = {National Bureau of Economic Research}",
      "}",
      sep = "\n"
    )
  )
)

provided_refs <- data.frame(
  key = c(
    "bisin2000beyond",
    "arellano2024genderpaygap",
    "bisin2023cultural",
    "goldin2024fertility",
    "goldin2016womenswork",
    "guiso2006culture",
    "imbens2024economic",
    "jayachandran2014genderinequality",
    "bezin2021brokenfamilies",
    "abramitzky2024culture",
    "28470",
    "olivetti2013female"
  ),
  pattern = c(
    "beyond the melting pot",
    "declining gender pay gap",
    "advances-in-the-economic-theory-of-cultural-transmission",
    "fertility and culture choices",
    "women's work and the gender wage gap|womenswork",
    "guiso et al culture and economic outcomes",
    "economic preferences and cultural transmission",
    "jayachandran roots of gender inequality",
    "crime broken families",
    "arrests and culture vector",
    "u-shaped female labor force",
    "2013 olivietti"
  ),
  stringsAsFactors = FALSE
)

if (!dir.exists(literature_root)) {
  stop(sprintf("Literature directory not found: %s", literature_root))
}

files <- sort(list.files(
  literature_root,
  pattern = "\\.[Pp][Dd][Ff]$",
  recursive = TRUE,
  full.names = TRUE
))

if (length(files) == 0) {
  stop("No PDF files were found in the literature directory.")
}

root_pattern <- paste0("^", escape_regex(normalizePath(literature_root)), "/")
rel_files <- sub(root_pattern, "", normalizePath(files), perl = TRUE)
base_lower <- tolower(basename(files))

used_keys <- character(0)
bib_chunks <- character(0)

for (i in seq_along(files)) {
  this_file <- files[[i]]
  this_rel <- rel_files[[i]]
  this_base <- tolower(basename(this_file))

  canon_idx <- which(vapply(canonical, function(x) {
    grepl(tolower(x$pattern), this_base, fixed = TRUE)
  }, logical(1)))

  if (length(canon_idx) > 0) {
    canon <- canonical[[canon_idx[[1]]]]
    bib_chunks <- c(
      bib_chunks,
      sprintf("%% source: %s", this_rel),
      canon$entry,
      ""
    )
    used_keys <- c(used_keys, canon$key)
    next
  }

  title <- title_from_filename(this_file)
  year <- extract_year(this_file)
  key <- make_key(title, year, used_keys)
  used_keys <- c(used_keys, key)

  entry_lines <- c(
    sprintf("@misc{%s,", key),
    sprintf("  title = {%s},", escape_bib(title))
  )
  if (nzchar(year)) {
    entry_lines <- c(entry_lines, sprintf("  year = {%s},", year))
  }
  entry_lines <- c(
    entry_lines,
    sprintf(
      "  note = {Metadata inferred from filename; verify bibliographic details. Source file: %s},",
      escape_bib(this_rel)
    ),
    "}"
  )

  bib_chunks <- c(
    bib_chunks,
    sprintf("%% source: %s", this_rel),
    entry_lines,
    ""
  )
}

writeLines(bib_chunks, out_bib, useBytes = TRUE)

match_file_for_pattern <- function(pattern) {
  p <- tolower(pattern)
  hits <- integer(0)
  if (grepl("\\|", p, perl = TRUE)) {
    parts <- strsplit(p, "\\|", perl = TRUE)[[1]]
    for (part in parts) {
      part <- trimws(part)
      if (!nzchar(part)) {
        next
      }
      hits <- c(hits, grep(part, base_lower, fixed = TRUE))
    }
    hits <- unique(hits)
  } else {
    hits <- grep(p, base_lower, fixed = TRUE)
  }
  if (length(hits) == 0) {
    return(list(found = FALSE, file = "", matches = 0))
  }
  list(found = TRUE, file = rel_files[[hits[[1]]]], matches = length(hits))
}

rows <- lapply(seq_len(nrow(provided_refs)), function(i) {
  m <- match_file_for_pattern(provided_refs$pattern[[i]])
  data.frame(
    key = provided_refs$key[[i]],
    in_folder = if (m$found) "yes" else "no",
    matched_file = if (m$found) m$file else "",
    possible_matches = as.character(m$matches),
    stringsAsFactors = FALSE
  )
})

cross <- do.call(rbind, rows)

report <- c(
  "# Tradwives Literature Cross-Check",
  "",
  sprintf("- Literature root scanned: `%s`", literature_root),
  sprintf("- PDFs found: **%d**", length(files)),
  sprintf("- BibTeX output: `%s`", out_bib),
  sprintf("- Generated on: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Status Against Provided Reference List",
  "",
  "| key | in folder | matched file | possible matches |",
  "|---|---|---|---|"
)

for (i in seq_len(nrow(cross))) {
  report <- c(
    report,
    sprintf(
      "| `%s` | %s | `%s` | %s |",
      cross$key[[i]],
      cross$in_folder[[i]],
      cross$matched_file[[i]],
      cross$possible_matches[[i]]
    )
  )
}

writeLines(report, out_report, useBytes = TRUE)

cat(sprintf("Wrote %s\n", out_bib))
cat(sprintf("Wrote %s\n", out_report))
cat(sprintf("PDF count: %d\n", length(files)))
