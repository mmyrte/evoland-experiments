#!/usr/bin/env bash
#
# Run a sub-experiment pipeline in order.
#
# Usage:
#   ./execute-all.sh [--workers N] '<glob>'
#
#   ./execute-all.sh '2026-05-ssp-ch/02*.qmd'             # everything matching, in order
#   ./execute-all.sh --workers 4 '2026-05-ssp-ch/02*.qmd' # up to 4 files of a stage at once
#
# Files are rendered/run in lexical order (stages are zero-padded, so a plain
# sort gives 020-... < 020d-... < 021-...). `.qmd` files are rendered with the
# Quarto CLI (which executes the embedded R and writes a self-contained HTML
# report next to the source); `.r`/`.R` files are run with Rscript (back-compat
# for sub-projects not yet converted to Quarto). Everything runs from the repo
# root so the root .Rprofile (rv activation) and relative paths resolve.
#
# Files sharing a leading number form one stage. Stages always run strictly one
# after another; within a stage the files are assumed independent (that is the
# repo convention for a shared stage number), so --workers N runs up to N of
# them at a time. Concurrency is safe because the evoland_db DuckLake catalog
# takes concurrent writers. `NNNd-*` diagnostics form their own stage, so they
# still see the finished stage they report on.
#
# Caveat: with --workers > 1 a stage's files run in no particular order. A step that
# depends on another step of the same stage is therefore a bug in the numbering, not
# something to work around with --workers 1: give the dependent step the next number
# (010-forcing-soil-download -> 011-forcing-soil-whc), never a slug suffix. See the
# repo README, "Three-digit numbering".
#
# With --workers 1 output streams as the step produces it. With more workers
# each step's output is captured and printed in one block when it finishes, so
# concurrent steps do not interleave. A failing step stops the pipeline: no new
# step is started, but steps already running are left to finish first.

set -euo pipefail

workers=1
while [[ $# -gt 1 ]]; do
  case "$1" in
    --workers|-j) workers="${2-}"; shift 2 ;;
    --workers=*|-j=*) workers="${1#*=}"; shift ;;
    -j[0-9]*) workers="${1#-j}"; shift ;;
    *) echo "Error: unknown option '$1'" >&2; exit 1 ;;
  esac
done

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 [--workers N] '<glob>'" >&2
  exit 1
fi
pattern="$1"

if [[ ! "$workers" =~ ^[1-9][0-9]*$ ]]; then
  echo "Error: --workers takes a positive integer, got '$workers'" >&2
  exit 1
fi

# The worker pool needs `wait -n`, which arrived in bash 4.3.
if (( workers > 1 && BASH_VERSINFO[0] * 100 + BASH_VERSINFO[1] < 403 )); then
  echo "Error: --workers > 1 needs bash >= 4.3, this is $BASH_VERSION" >&2
  exit 1
fi

shopt -s nullglob
files=( $pattern )
shopt -u nullglob

if [[ ${#files[@]} -eq 0 ]]; then
  echo "Error: No files matching pattern '$pattern'" >&2
  exit 1
fi

# Every file needs a leading number to place it in a stage, and an extension we
# know how to run. Checked up front, so nothing starts if any match is unrunnable.
for file in "${files[@]}"; do
  base="$(basename "$file")"
  if [[ ! "$base" =~ ^[0-9] ]]; then
    echo "Error: File '$base' does not start with a number" >&2
    exit 1
  fi
  case "$base" in
    *.qmd | *.QMD | *.r | *.R) ;;
    *) echo "Error: don't know how to run '$base'" >&2; exit 1 ;;
  esac
done

# Lexical sort (LC_ALL=C); zero-padded stages make this the correct run order.
mapfile -t sorted_files < <(printf '%s\n' "${files[@]}" | LC_ALL=C sort)

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

tmpdir=""
cleanup() {
  if [[ -n "$tmpdir" ]]; then
    rm -rf "$tmpdir"
  fi
}
trap cleanup EXIT

# Stage of a file: its leading number, plus the `d` of a diagnostic so those
# form a stage of their own. Sorting the keys with LC_ALL=C keeps the same order
# the file names themselves sort in (020 < 020d < 021).
stage_key() {
  local base="$1"
  [[ "$base" =~ ^([0-9]+d?) ]] || return 1
  printf '%s\n' "${BASH_REMATCH[1]}"
}

run_file() {
  local file="$1"
  case "$file" in
    *.qmd | *.QMD)
      (cd "$script_dir" && quarto render "$file") ;;
    *.r | *.R)
      (cd "$script_dir" && Rscript "$file") ;;
    *)
      echo "Error: don't know how to run '$(basename "$file")'" >&2; return 2 ;;
  esac
}

# Runs one stage's files, at most $workers at a time, and fails if any of them
# did. Each concurrent step writes to its own log, printed when the step ends.
run_stage() {
  local stage="$1"
  shift
  local -a group=( "$@" )
  local n=${#group[@]}
  local file base

  if (( workers == 1 || n == 1 )); then
    for file in "${group[@]}"; do
      echo "==> $(basename "$file")"
      run_file "$file"
    done
    return 0
  fi

  echo "== stage $stage: $n files, up to $workers at a time"
  local logdir="$tmpdir/$stage"
  mkdir -p "$logdir"

  local -a started=() reported=()
  local i next=0 running=0 failed=0 reaped=0 status wait_status=0
  for (( i = 0; i < n; i++ )); do
    started[i]=false
    reported[i]=false
  done

  while (( next < n || running > 0 )); do
    while (( running < workers && next < n )); do
      echo "--> $(basename "${group[next]}") started"
      # `|| step_status=$?` keeps the inherited errexit from killing the
      # subshell before it has recorded the status of a failing step.
      (
        step_status=0
        run_file "${group[next]}" >"$logdir/$next.log" 2>&1 || step_status=$?
        echo "$step_status" >"$logdir/$next.status"
      ) &
      started[next]=true
      running=$(( running + 1 ))
      next=$(( next + 1 ))
    done

    wait_status=0
    wait -n 2>/dev/null || wait_status=$?

    # A step writes its status file last, so its log is complete by then.
    reaped=0
    for (( i = 0; i < n; i++ )); do
      if ! ${started[i]} || ${reported[i]} || [[ ! -f "$logdir/$i.status" ]]; then
        continue
      fi
      base="$(basename "${group[i]}")"
      status="$(cat "$logdir/$i.status")"
      echo "==> $base"
      cat "$logdir/$i.log"
      if (( status != 0 )); then
        echo "Error: '$base' exited with status $status" >&2
        failed=1
      fi
      reported[i]=true
      reaped=$(( reaped + 1 ))
      running=$(( running - 1 ))
    done

    # No children left and nothing to reap, yet steps are outstanding: they were
    # killed before recording a status. Report them instead of looping forever.
    if (( wait_status == 127 && reaped == 0 && running > 0 )); then
      for (( i = 0; i < n; i++ )); do
        if ! ${started[i]} || ${reported[i]}; then
          continue
        fi
        base="$(basename "${group[i]}")"
        echo "Error: '$base' was killed before it reported a status" >&2
        if [[ -f "$logdir/$i.log" ]]; then
          echo "==> $base"
          cat "$logdir/$i.log"
        fi
        failed=1
        reported[i]=true
        running=$(( running - 1 ))
      done
    fi

    # Start nothing new after a failure, but let the running steps finish.
    if (( failed )); then
      next=$n
    fi
  done

  if (( failed )); then
    return 1
  fi
  return 0
}

if (( workers > 1 )); then
  tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/execute-all.XXXXXX")"
fi

mapfile -t stages < <(
  for file in "${sorted_files[@]}"; do
    stage_key "$(basename "$file")"
  done | LC_ALL=C sort -u
)

for stage in "${stages[@]}"; do
  stage_files=()
  for file in "${sorted_files[@]}"; do
    if [[ "$(stage_key "$(basename "$file")")" == "$stage" ]]; then
      stage_files+=( "$file" )
    fi
  done
  run_stage "$stage" "${stage_files[@]}"
done

echo "All pipeline steps completed successfully"
