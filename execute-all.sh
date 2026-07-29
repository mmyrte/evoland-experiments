#!/usr/bin/env bash
#
# Run a sub-experiment pipeline in order.
#
# Usage:
#   ./execute-all.sh [--core|--diagnostics] '<glob>'
#
#   ./execute-all.sh '2026-05-ssp-ch/0*.qmd'        # everything matching, in order
#   ./execute-all.sh --core '2026-05-ssp-ch/0*.qmd' # skip NNd-* diagnostics
#   ./execute-all.sh --diagnostics '2026-05-ssp-ch/0*.qmd' # only NNd-* diagnostics
#
# Files are rendered/run in lexical order (stages are zero-padded, so a plain
# sort gives 02-... < 02d-... < 03-...). `.qmd` files are rendered with the
# Quarto CLI (which executes the embedded R and writes a self-contained HTML
# report next to the source); `.r`/`.R` files are run with Rscript (back-compat
# for sub-projects not yet converted to Quarto). Everything runs from the repo
# root so the root .Rprofile (rv activation) and relative paths resolve.

set -euo pipefail

mode="all" # all | core | diagnostics
while [[ $# -gt 1 ]]; do
  case "$1" in
    --core) mode="core"; shift ;;
    --diagnostics) mode="diagnostics"; shift ;;
    *) echo "Error: unknown option '$1'" >&2; exit 1 ;;
  esac
done

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 [--core|--diagnostics] '<glob>'" >&2
  exit 1
fi
pattern="$1"

shopt -s nullglob
files=( $pattern )
shopt -u nullglob

if [[ ${#files[@]} -eq 0 ]]; then
  echo "Error: No files matching pattern '$pattern'" >&2
  exit 1
fi

# Validate leading number, and filter by mode (NNd-* == diagnostic).
selected=()
for file in "${files[@]}"; do
  base="$(basename "$file")"
  if [[ ! "$base" =~ ^[0-9] ]]; then
    echo "Error: File '$base' does not start with a number" >&2
    exit 1
  fi
  is_diag=false
  [[ "$base" =~ ^[0-9]+d- ]] && is_diag=true
  case "$mode" in
    core) $is_diag && continue ;;
    diagnostics) $is_diag || continue ;;
  esac
  selected+=( "$file" )
done

if [[ ${#selected[@]} -eq 0 ]]; then
  echo "Error: No files left after --$mode filter for pattern '$pattern'" >&2
  exit 1
fi

# Lexical sort (LC_ALL=C); zero-padded stages make this the correct run order.
mapfile -t sorted_files < <(printf '%s\n' "${selected[@]}" | LC_ALL=C sort)

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

for file in "${sorted_files[@]}"; do
  base="$(basename "$file")"
  echo "==> $base"
  case "$file" in
    *.qmd|*.QMD)
      (cd "$script_dir" && quarto render "$file") ;;
    *.r|*.R)
      (cd "$script_dir" && Rscript "$file") ;;
    *)
      echo "Error: don't know how to run '$base'" >&2; exit 1 ;;
  esac
done

echo "All pipeline steps completed successfully"
