#!/usr/bin/env bash
# Loop running the "open close" test until a Broken pipe is observed.
# Writes each iteration's full output to test-logs/open-close-loop-<n>.log
# Environment you can tweak:
#   MAX_ITER   : maximum iterations before giving up (default: unlimited)
#   SLEEP_SECS : seconds to sleep between iterations (default: 0)
#   TRACE_FD   : set to 1 to enable HLS_TEST_TRACE_FD (default: 1)
#   LOG_STDERR : set to 1 to enable verbose stderr logging (HLS_TEST_LOG_STDERR & HLS_TEST_HARNESS_STDERR) (default: 1)
#
# Exit codes:
#   0 on success (broken pipe reproduced)
#   1 on reaching MAX_ITER without reproduction
#   2 on other setup error

set -euo pipefail

MAX_ITER="${MAX_ITER:-}"
SLEEP_SECS="${SLEEP_SECS:-0}"
TRACE_FD="${TRACE_FD:-1}"
LOG_STDERR="${LOG_STDERR:-1}"

# Allow providing a positional max iteration: ./open-close-loop.sh 50
if [[ $# -ge 1 && -z "${MAX_ITER}" ]]; then
  MAX_ITER="$1"
fi

mkdir -p test-logs

iter=0
start_ts=$(date -Iseconds)
echo "[loop] Starting at ${start_ts}" >&2

# Pattern string to detect (keep simple & literal for robustness)
BROKEN_PIPE_RE='Broken pipe'
DEBUG_DETECT="${DEBUG_DETECT:-0}"

if [[ -z "${NO_BUILD_ONCE:-}" ]]; then
  echo "[loop] Building test target ghcide-tests once upfront" >&2
  cabal build ghcide-tests >&2
fi

# Locate the built test binary (simple heuristic similar to run_progress_test.sh)
if [[ -z "${TEST_BIN:-}" ]]; then
  TEST_BIN=$(find dist-newstyle -type f -name ghcide-tests -perm -111 2>/dev/null | head -n1 || true)
fi

if [[ -z "${TEST_BIN}" || ! -x "${TEST_BIN}" ]]; then
  echo "[loop][error] Could not locate executable test binary 'ghcide-tests'. Set TEST_BIN explicitly or ensure build succeeded." >&2
  exit 2
fi
echo "[loop] Using test binary: ${TEST_BIN}" >&2

REBUILD_EACH="${REBUILD_EACH:-0}" # set to 1 to rebuild before every iteration

while true; do
  iter=$((iter+1))
  ts=$(date -Iseconds)
  log="test-logs/open-close-loop-${iter}.log"
  echo "[loop] Iteration ${iter} starting at ${ts}, logging to ${log}" | tee -a "${log}" >&2
  # Run the single test pattern. We don't fail the loop on non-zero exit (capture output then decide).
  set +e
  if [[ ${REBUILD_EACH} -eq 1 ]]; then
    echo "[loop] Rebuilding before iteration ${iter}" | tee -a "${log}" >&2
    cabal build ghcide-tests >>"${log}" 2>&1
    # refresh TEST_BIN if path changed
    TEST_BIN_NEW=$(find dist-newstyle -type f -name ghcide-tests -perm -111 2>/dev/null | head -n1 || true)
    if [[ -n "${TEST_BIN_NEW}" ]]; then TEST_BIN="${TEST_BIN_NEW}"; fi
  fi
  HLS_TEST_TRACE_FD="${TRACE_FD}" \
  HLS_TEST_LOG_STDERR="${LOG_STDERR}" \
  HLS_TEST_HARNESS_STDERR="${LOG_STDERR}" \
  TASTY_NUM_THREADS=1 \
  TASTY_PATTERN="open close" \
  "${TEST_BIN}" >"${log}" 2>&1
  ec=$?
  set -e

  if grep -aFq -- "${BROKEN_PIPE_RE}" "${log}"; then
    echo "[loop] Broken pipe reproduced in iteration ${iter}. Stopping." | tee -a "${log}" >&2
    echo "[loop] --- Tail (last 60 lines) ---" >&2
    tail -n 60 "${log}" >&2
    exit 0
  else
    if [[ ${DEBUG_DETECT} -eq 1 ]]; then
      echo "[loop][debug] No match for '${BROKEN_PIPE_RE}' in iteration ${iter}." | tee -a "${log}" >&2
    fi
  fi

  if [[ -n "${MAX_ITER}" && ${iter} -ge ${MAX_ITER} ]]; then
    echo "[loop] Reached MAX_ITER=${MAX_ITER} without reproducing Broken pipe." >&2
    exit 1
  fi

  echo "[loop] Iteration ${iter} complete (exit code ${ec}). No Broken pipe yet." | tee -a "${log}" >&2
  if [[ ${SLEEP_SECS} -gt 0 ]]; then
    echo "[loop] Sleeping ${SLEEP_SECS}s" | tee -a "${log}" >&2
    sleep "${SLEEP_SECS}"
  fi
done
