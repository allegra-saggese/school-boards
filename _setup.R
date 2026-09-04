# Shared preamble. Source this first in every script:
#
#     source(here::here("_setup.R"))
#
# here::here() resolves against the repo root (found via school-boards.Rproj),
# so scripts run correctly from any working directory — including from inside
# t1/, t2/ and t3/.
if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install it with install.packages('here').")
}
source(here::here("R", "paths.R"))
source(here::here("functions.R"))
