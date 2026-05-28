#!/usr/bin/env bash
# Pass 2: cost-centre profiled benchmark.
#
# Requires bench-results/binaries/HEAD-prof/haskell-language-server to exist
# (built with --enable-profiling --profiling-detail=late).
#
# For each (example, experiment) pair, invoke ghcide-bench directly with
# +RTS -p -hc -i1 -po<stem> -RTS so we get:
#   <stem>.prof       — time + allocation by cost-centre
#   <stem>.hp         — heap residency by cost-centre over time
#   <stem>.hp.csv     — top-N cost-centres (produced by parseHpProfile, but
#                       NOTE: only the bench harness invokes that. For Pass 2
#                       we'd need to run summarizeHpProfile separately. See
#                       end of script.)
set -euo pipefail

HLS=bench-results/binaries/HEAD-prof/haskell-language-server
OUTDIR=bench-results/prof-cc
SAMPLES=20

if [[ ! -x "$HLS" ]]; then
    echo "ERROR: profiled HLS not found at $HLS" >&2
    exit 1
fi

run_experiment() {
    local example_name=$1
    local experiment=$2
    shift 2
    local example_args=("$@")

    local exp_slug=${experiment// /_}
    local outdir="$OUTDIR/$example_name/$exp_slug"
    mkdir -p "$outdir"
    local stem="$(pwd)/$outdir/$exp_slug"

    echo
    echo "=== $example_name :: $experiment ==="

    cabal exec -- ghcide-bench \
        --timeout=600 \
        --no-clean \
        --samples="$SAMPLES" \
        --csv="$outdir/$exp_slug.csv" \
        --ghcide="$(pwd)/$HLS" \
        --select "$experiment" \
        "${example_args[@]}" \
        --ghcide-options=+RTS \
        --ghcide-options=-p \
        --ghcide-options=-hc \
        --ghcide-options=-i1 \
        --ghcide-options=-po"$stem" \
        --ghcide-options=-l \
        --ghcide-options=-ol"$stem.eventlog" \
        --ghcide-options=-S"$stem.gcStats.log" \
        --ghcide-options=-RTS \
        < /dev/null || echo "  FAILED — continuing"
}

# ---------- Example: cabal-3.16.1.0 ----------
CABAL_ARGS=(
    --example-package-name Cabal
    --example-package-version 3.16.1.0
    --example-name cabal
    --example-module=src/Distribution/Simple.hs
    --example-module=src/Distribution/Types/ComponentLocalBuildInfo.hs
)
for exp in "memory pressure" "completions after typing burst" "hover after typing burst" "edit" "semanticTokens after typing burst"; do
    run_experiment cabal "$exp" "${CABAL_ARGS[@]}"
done

# ---------- Example: MultiLayerModulesNoTH ----------
MLM_ARGS=(
    --example-script "$(pwd)/bench/MultiLayerModules.sh"
    --example-name MultiLayerModulesNoTH
    --example-module=MultiLayerModules.hs
    --example-module=DummyLevel0M01.hs
    --example-module=DummyLevel1M01.hs
)
for exp in "memory pressure" "edit" "hover after typing burst"; do
    run_experiment MultiLayerModulesNoTH "$exp" "${MLM_ARGS[@]}"
done

echo
echo "=== Pass 2 complete ==="
echo "Profile artefacts under $OUTDIR/"
echo "Inspect .prof files for time+alloc attribution: head -50 $OUTDIR/cabal/memory_pressure/memory_pressure.prof"
