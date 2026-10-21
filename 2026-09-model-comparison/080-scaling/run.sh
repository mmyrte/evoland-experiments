#!/usr/bin/env bash
#
# Scaling benchmark: run each tool's pipeline step on PIE tiled k x k, one realisation, each
# step its own process under /usr/bin/time -v (wall time, peak resident memory). Resumable: a
# step that finished (or failed, or timed out) for a k is not rerun; delete its marker in
# outputs-k<k>/scaling/ to repeat it.
#
# Usage (repo root): 2026-09-model-comparison/080-scaling/run.sh "1 2 3 4 6" [timeout_s]
# Needs CLUINPY_REPO and CLUINPY_PYTHON as for 040-alloc-cluinpy.r. The lulcc and CLUinPy steps
# allocate the logistic regression's surface and run the tool as shipped; their timings are
# recorded per stage, so collect.r can tell the two apart.

set -uo pipefail
scales="${1:-1 2 3 4 6}"
step_timeout="${2:-3600}"
dir="2026-09-model-comparison"
export R_PROFILE_USER=/dev/null PIE_N_REALISATIONS=1 PIE_ESTIMATORS=log_reg

steps=(
  000-pie-data.r
  010-evoland-calibrate.r
  015-dinamica-woe.r
  020-potentials.r
  030-alloc-evoland-clumpy.r
  030-alloc-evoland-dinamica.r
  040-alloc-lulcc.r
  040-alloc-cluinpy.r
)

for k in $scales; do
  export PIE_SCALE="$k"
  suffix="-k$k"
  marks="$dir/outputs$suffix/scaling"
  mkdir -p "$marks"
  for step in "${steps[@]}"; do
    marker="$marks/$step.done"
    [[ -f "$marker" ]] && continue
    echo "== k=$k $step"
    log="$marks/$step.log"
    status=0
    /usr/bin/time -v -o "$marks/$step.time" timeout "$step_timeout" Rscript "$dir/$step" > "$log" 2>&1 || status=$?
    wall=$(grep "Elapsed (wall clock)" "$marks/$step.time" | awk '{print $NF}')
    rss=$(grep "Maximum resident set size" "$marks/$step.time" | awk '{print $NF}')
    echo "$k,$step,$status,$wall,$rss" > "$marker"
    echo "   status=$status wall=$wall peak_rss_kb=$rss"
    # a failed calibration leaves nothing for the evoland allocation steps
    if [[ "$step" == 010-* && "$status" -ne 0 ]]; then
      for s in 020-potentials.r 030-alloc-evoland-clumpy.r 030-alloc-evoland-dinamica.r; do
        echo "$k,$s,skipped,," > "$marks/$s.done"
      done
    fi
  done
done
echo "scaling done"
