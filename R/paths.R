# Path helpers for the project.
# Uses config.yml and/or environment variables to keep file locations flexible.

suppressPackageStartupMessages({
  if (requireNamespace("here", quietly = TRUE)) {
    library(here)
  }
  if (requireNamespace("yaml", quietly = TRUE)) {
    library(yaml)
  }
})

repo_root <- function() {
  if ("here" %in% .packages()) {
    return(here::here())
  }
  normalizePath(getwd())
}

read_config <- function() {
  cfg_path <- file.path(repo_root(), "config.yml")
  if (file.exists(cfg_path) && "yaml" %in% .packages()) {
    return(yaml::read_yaml(cfg_path))
  }
  list()
}

get_cfg <- function(key, default = NULL) {
  cfg <- read_config()
  if (!is.null(cfg[[key]])) {
    return(cfg[[key]])
  }
  default
}

data_root <- function() {
  env <- Sys.getenv("SCHOOL_BOARDS_DATA_ROOT")
  if (nzchar(env)) {
    return(env)
  }
  file.path(repo_root(), get_cfg("data_root", "data"))
}

external_root <- function() {
  env <- Sys.getenv("SCHOOL_BOARDS_EXTERNAL_ROOT")
  if (nzchar(env)) {
    return(env)
  }
  get_cfg("external_data_root", "")
}

graphs_dir <- function() {
  file.path(repo_root(), get_cfg("graphs_dir", "data/graphs"))
}

data_path <- function(...) {
  file.path(data_root(), ...)
}

ext_path <- function(...) {
  root <- external_root()
  if (!nzchar(root)) {
    stop("External data root not set. Set SCHOOL_BOARDS_EXTERNAL_ROOT or config.yml external_data_root.")
  }
  file.path(root, ...)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(path)
}
