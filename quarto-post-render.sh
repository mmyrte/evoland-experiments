#!/usr/bin/env bash
#
# Quarto post-render hook: file each rendered report under an `html-reports/`
# folder beside its source, so the step directories hold sources only.
#
# Quarto always writes a report next to its .qmd and offers no per-directory
# output location (`output-dir` is project-wide and mirrors the input tree;
# `output-file` rejects a path). Moving the file afterwards is therefore the
# way to get one report folder per sub-experiment. `embed-resources: true`
# makes every report a single self-contained file, so the move is just a
# rename; the `*_files/` sidecar is handled anyway in case that is ever off.
#
# Quarto runs this from the project root for both `quarto render <file>` and a
# whole-project render, passing the outputs as a newline-separated list of
# project-relative paths in QUARTO_PROJECT_OUTPUT_FILES.

set -euo pipefail

if [[ -z "${QUARTO_PROJECT_OUTPUT_FILES:-}" ]]; then
  exit 0
fi

while IFS= read -r out; do
  if [[ -z "$out" || "$out" != *.html || ! -f "$out" ]]; then
    continue
  fi

  dir="$(dirname "$out")"
  base="$(basename "$out")"

  # Nothing to do for a report that is already in place.
  if [[ "$(basename "$dir")" == "html-reports" ]]; then
    continue
  fi

  dest="$dir/html-reports"
  mkdir -p "$dest"
  mv -f "$out" "$dest/$base"

  sidecar="${base%.html}_files"
  if [[ -d "$dir/$sidecar" ]]; then
    rm -rf "${dest:?}/$sidecar"
    mv -f "$dir/$sidecar" "$dest/"
  fi

  echo "Report filed at $dest/$base"
done <<< "$QUARTO_PROJECT_OUTPUT_FILES"
