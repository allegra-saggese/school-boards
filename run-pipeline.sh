#!/bin/zsh
# =============================================================================
# Re-run the analysis pipeline end to end, in dependency order.
#
#   ./run-pipeline.sh            run everything
#   ./run-pipeline.sh t3         run only scripts whose path matches "t3"
#   ./run-pipeline.sh --list     print the run order and exit
#
# Logs go to logs/<script>.log, one per script, and a pass/fail table is
# written to logs/summary.txt. A non-zero exit code in that table is a failure.
#
# NOT RUN HERE — these three have external side effects or need a key that the
# analysis scripts do not, so run them deliberately, by hand:
#   ipums-bkp-build-database.R     re-downloads IPUMS extract #4 and rebuilds
#                                  the ~19GB data/interim/ipums_bkp.sqlite
#   ipums-submit-housing-extract.R submits a NEW extract request to IPUMS;
#                                  their API has no delete endpoint, so a test
#                                  run leaves a real orphaned extract behind
#   fred-county-panel.R            needs FRED_API_KEY and re-downloads
#
# Everything below reads data already on disk.
# =============================================================================
cd "${0:A:h}" || exit 1

LOG=logs
FILTER="${1:-}"

SCRIPTS=(
  # build layer — shared, feeds more than one part
  lfpr-groupings.R
  ipums-county-female-lfpr-scatter.R
  ipums-wage-quintile-time-graphs.R
  ipums-spouse-income-scatter-plots.R
  ipums-build-housing-merge.R
  ipums-married-household-suite.R
  # T1 — BKP replication
  t1/t1-replication.R
  t1/t1-summary-outputs.R
  t1/t1-augmented-tests.R
  t1/t1-figures.R
  # T2 — culture x wealth quadrant
  t2/t2-empirical-quadrant.R
  t2/t2-figures.R
  t2/t2-rdd-breadwinner-norm.R
  t2/t2-ols-regressions.R
  # T3 — structural model
  t3/t3-estimate-v2.R
  t3/t3-compute-tau.R
  t3/t3-aggregate-distortion.R
  t3/t3-comparative-statics.R
  t3/t3-social-multiplier.R
  t3/t3-estimate-v3.R
  t3/t3-figures.R
)

if [[ "$FILTER" == "--list" ]]; then
  printf '%s\n' $SCRIPTS
  exit 0
fi

# Years the T3 estimation covers — the FULL series by default.
#
# Do not narrow this casually. t3-estimate-v2.R writes a dated estimates file,
# and every downstream T3 script reads the most recent one, so a short run
# silently shadows the full series: tau, the aggregate distortion and all six
# figures get rebuilt from a handful of points. t3-comparative-statics.R also
# hardcodes YR <- 2019 and fails outright if that year is missing.
#
# Override only for a deliberate quick check, and archive the partial estimates
# afterwards so they do not shadow the real ones.
: ${T3_YEARS:=1980,1990,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024}
export T3_YEARS

mkdir -p $LOG
: > $LOG/summary.txt
failed=0

for script in $SCRIPTS; do
  [[ -n "$FILTER" && "$script" != *"$FILTER"* ]] && continue
  start=$SECONDS
  Rscript "$script" > "$LOG/${script//\//_}.log" 2>&1
  rc=$?
  printf '%-38s rc=%-3s %5ss\n' "$script" "$rc" "$((SECONDS-start))" \
    | tee -a $LOG/summary.txt
  (( rc != 0 )) && (( failed++ ))
done

echo "---" | tee -a $LOG/summary.txt
if (( failed )); then
  echo "$failed script(s) FAILED — see $LOG/" | tee -a $LOG/summary.txt
  exit 1
fi
echo "all scripts completed" | tee -a $LOG/summary.txt
