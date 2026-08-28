#!/usr/bin/env bash
# Rebuild the manuscript from the archived bytes, failing closed at every stage.
#
#   bash build.sh
#
# Stage 1 re-hashes every artifact the manuscript depends on and stops if any
# byte differs from the digest recorded in the manifest. Stage 2 regenerates the
# figures and every printed number from those bytes. Stage 3 typesets. Nothing
# in the manuscript is transcribed by hand, so a stale artifact cannot survive
# as a plausible-looking number.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

echo "==> [1/3] re-verifying archived evidence"
python3 tools/bind_evidence.py paper --check

echo "==> [2/3] regenerating figures and generated tex"
(cd paper && Rscript figs/make_figs.R)

echo "==> [3/3] compiling the manuscript"
rm -rf paper/build
LOG="$(mktemp)"
trap 'rm -f "$LOG"' EXIT
if ! latexmk -pdf -cd -interaction=nonstopmode -halt-on-error \
     -outdir=../build paper/tex/main.tex >"$LOG" 2>&1; then
  echo "FAIL: latexmk exited non-zero" >&2
  tail -40 "$LOG" >&2
  exit 1
fi

if grep -q "Citation.*undefined" paper/build/main.log 2>/dev/null; then
  echo "FAIL: the manuscript cites something the bibliography does not define" >&2
  exit 1
fi

echo "OK  paper/build/main.pdf"
