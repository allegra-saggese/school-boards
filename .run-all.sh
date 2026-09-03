#!/bin/zsh
# Temporary verification runner — not part of the repo.
# Runs every live script against the data already on disk and records the outcome.
cd "$(dirname "$0")" || exit 1

LOG=/tmp/runall
mkdir -p $LOG
: > $LOG/summary.txt

export T2_CACHE_WRITE="$PWD/data/interim/t2_quadrant.rds"
export T2_CACHE="$PWD/data/interim/t2_quadrant.rds"
export T3_YEARS="1980,2000,2024"

run () {
  local script="$1" limit="$2"
  local name="${script//\//_}"
  local start=$SECONDS
  # gtimeout if present, else plain run
  if command -v gtimeout >/dev/null 2>&1; then
    gtimeout "$limit" Rscript "$script" > "$LOG/$name.log" 2>&1
  else
    Rscript "$script" > "$LOG/$name.log" 2>&1
  fi
  local rc=$? el=$((SECONDS-start))
  printf "%-40s rc=%-4s %5ss\n" "$script" "$rc" "$el" >> $LOG/summary.txt
  printf "%-40s rc=%-4s %5ss\n" "$script" "$rc" "$el"
}

# ---- build layer (reads data already on disk) ----
run lfpr-groupings.R                        1800
run ipums-county-female-lfpr-scatter.R      1800
run ipums-wage-quintile-time-graphs.R       1800
run ipums-spouse-income-scatter-plots.R     1800
run ipums-build-housing-merge.R             1800
run ipums-married-household-suite.R         3600

# ---- T1 ----
run t1/t1-replication.R                     7200
run t1/t1-summary-outputs.R                 7200
run t1/t1-augmented-tests.R                 3600
run t1/t1-figures.R                         1800

# ---- T2 ----
run t2/t2-empirical-quadrant.R              5400
run t2/t2-figures.R                         1800
run t2/t2-rdd-breadwinner-norm.R            3600
run t2/t2-ols-regressions.R                 1800

# ---- T3 ----
run t3/t3-estimate-v2.R                     5400
run t3/t3-compute-tau.R                     3600
run t3/t3-aggregate-distortion.R            3600
run t3/t3-comparative-statics.R             3600
run t3/t3-social-multiplier.R               1800
run t3/t3-estimate-v3.R                     5400
run t3/t3-figures.R                         3600

echo "DONE" >> $LOG/summary.txt
