#!/usr/bin/env bash
# Loop running HLS tasty tests until a Broken pipe or test failure is observed.
# Originally ran only the "open close" test; now supports multiple patterns.
# Logs each run to test-logs/<pattern-slug>-loop-<n>.log, rotating every 100 files per pattern.
#
# Environment you can tweak:
#   MAX_ITER      : maximum iterations before giving up (default: 1000)
#   SLEEP_SECS    : seconds to sleep between iterations (default: 0)
#   SHOW_EVERY    : print progress/iteration header every N iterations (default: 100, 1 = every run, <=0 = disabled)
#   LOG_STDERR    : set to 1 to enable verbose stderr logging (HLS_TEST_LOG_STDERR & HLS_TEST_HARNESS_STDERR) (default: 1)
#   TEST_BIN      : path to the built test binary (auto-discovered if not set)
#   NO_BUILD_ONCE : set to non-empty to skip the initial cabal build step
#
# Test selection:
#   TEST_PATTERNS : comma-separated list of tasty patterns to run each iteration.
#                   Example: TEST_PATTERNS='open close,bidirectional module dependency with hs-boot'
#                   If set and non-empty, this takes precedence over PATTERN_FILE.
#                   If unset, defaults to 'open close' to match prior behavior.
#   PATTERN_FILE  : path to a file with one pattern per line (lines starting with # or blank are ignored).
#                   Used only if TEST_PATTERNS is empty/unset; otherwise ignored.
#
# Exit codes:
#   1 on success (broken pipe or test failure reproduced)
#   0 on reaching MAX_ITER without reproduction
#   2 on other setup error

set -euo pipefail

MAX_ITER="${MAX_ITER:-}"
SLEEP_SECS="${SLEEP_SECS:-0}"
SHOW_EVERY="${SHOW_EVERY:-1}"
LOG_STDERR="${LOG_STDERR:-1}"

# Allow providing a positional max iteration: ./open-close-loop.sh 50
if [[ $# -ge 1 && -z "${MAX_ITER}" ]]; then
  MAX_ITER="$1"
fi

# fallback to default if not set
if [[ -z "${MAX_ITER}" ]]; then
  MAX_ITER=1000
fi

mkdir -p test-logs

iter=0
start_ts=$(date -Iseconds)
echo "[loop] Starting at ${start_ts}" >&2

# Pattern strings to detect issues (keep simple & literal for robustness)
BROKEN_PIPE_RE='Broken pipe'
TEST_FAILED_RE='fail'
DEBUG_DETECT="${DEBUG_DETECT:-0}"

# Resolve which tasty patterns to run each iteration
patterns=()
if [[ -n "${TEST_PATTERNS:-}" ]]; then
  IFS=',' read -r -a patterns <<< "${TEST_PATTERNS}"
  # trim whitespace and drop empty entries
  tmp_patterns=()
  for p in "${patterns[@]}"; do
    # trim leading
    p="${p#${p%%[![:space:]]*}}"
    # trim trailing
    p="${p%${p##*[![:space:]]}}"
    [[ -z "$p" ]] && continue
    tmp_patterns+=("$p")
  done
  patterns=("${tmp_patterns[@]}")
elif [[ -n "${PATTERN_FILE:-}" && -r "${PATTERN_FILE}" ]]; then
  while IFS= read -r line; do
    # trim whitespace, skip comments and blank lines
    trimmed="${line#${line%%[![:space:]]*}}"
    trimmed="${trimmed%${trimmed##*[![:space:]]}}"
    [[ -z "${trimmed}" || "${trimmed}" =~ ^[[:space:]]*# ]] && continue
    patterns+=("${trimmed}")
  done < "${PATTERN_FILE}"
else
  # default to the original single test
  patterns+=("open close")
fi

if [[ ${#patterns[@]} -eq 0 ]]; then
  echo "[loop][error] No test patterns provided (via PATTERN_FILE or TEST_PATTERNS)." >&2
  exit 2
fi

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

while true; do
  iter=$((iter+1))
  ts=$(date -Iseconds)
  file_num=$((iter % 100))
  if [[ ${file_num} -eq 0 ]]; then file_num=100; fi

  # Run each selected pattern in this iteration
  for pattern in "${patterns[@]}"; do
    # sanitize pattern for a log slug
    slug=$(printf '%s' "${pattern}" | tr -cs 'A-Za-z0-9._-' '-' | sed -E 's/^-+|-+$//g')
    [[ -z "${slug}" ]] && slug="pattern"
    log="test-logs/${slug}-loop-${file_num}.log"

  # Show iteration start at first run and then every SHOW_EVERY runs (if > 0)
  if [[ ${iter} -eq 1 || ( ${SHOW_EVERY} -gt 0 && $((iter % SHOW_EVERY)) -eq 0 ) ]]; then
      echo "[loop] Iteration ${iter} (${ts}) pattern='${pattern}' -> ${log}" | tee -a "${log}" >&2
    fi

    # We don't fail the loop on non-zero exit (capture output then decide).
  set +e
    # HLS_TEST_HARNESS_NO_TESTDIR_CLEANUP=1 \
    HLS_TEST_LOG_STDERR="${LOG_STDERR}" \
    HLS_TEST_HARNESS_STDERR="${LOG_STDERR}" \
    TASTY_NUM_THREADS=1 \
    TASTY_PATTERN="${pattern}" \
    "${TEST_BIN}" >"${log}" 2>&1
    set -e

    if grep -aFq -- "${BROKEN_PIPE_RE}" "${log}"; then
      echo "[loop] Broken pipe reproduced in iteration ${iter} for pattern '${pattern}'. Stopping." | tee -a "${log}" >&2
      echo "[loop] Log file: ${log} (abs: $(pwd)/${log})" | tee -a "${log}" >&2
      echo "[loop] --- Tail (last 60 lines) ---" >&2
      tail -n 60 "${log}" >&2
      exit 1
    elif grep -aFq -- "${TEST_FAILED_RE}" "${log}"; then
      echo "[loop] Test failure detected in iteration ${iter} for pattern '${pattern}'. Stopping." | tee -a "${log}" >&2
      echo "[loop] Log file: ${log} (abs: $(pwd)/${log})" | tee -a "${log}" >&2
      echo "[loop] --- Tail (last 60 lines) ---" >&2
      tail -n 60 "${log}" >&2
      exit 1
    else
      if [[ ${DEBUG_DETECT} -eq 1 ]]; then
        echo "[loop][debug] No match for '${BROKEN_PIPE_RE}' or '${TEST_FAILED_RE}' in iteration ${iter} (pattern='${pattern}')." | tee -a "${log}" >&2
      fi
    fi
  done

  if [[ -n "${MAX_ITER}" && ${iter} -ge ${MAX_ITER} ]]; then
    echo "[loop] Reached MAX_ITER=${MAX_ITER} without reproducing issues." >&2
    exit 0
  fi

  # Show progress at the configured cadence
  if [[ ${SHOW_EVERY} -gt 0 && $((iter % SHOW_EVERY)) -eq 0 ]]; then
    echo "[loop] Progress: Completed ${iter} iterations without detecting issues." >&2
  fi

  if [[ ${SLEEP_SECS} -gt 0 ]]; then
  echo "[loop] Sleeping ${SLEEP_SECS}s" >&2
    sleep "${SLEEP_SECS}"
  fi
done
