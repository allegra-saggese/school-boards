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
