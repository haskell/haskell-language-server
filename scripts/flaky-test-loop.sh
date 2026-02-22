#!/usr/bin/env bash
# Loop running HLS tasty tests until a Broken pipe or test failure is observed.
# Originally ran only the "open close" test; now supports multiple patterns.
# Ensures successful build before running any tests.
# Logs each run to test-logs/<pattern-slug>-loop-<n>.log, rotating every 100 files per pattern.
#
# Environment you can tweak:
#   MAX_ITER      : maximum iterations before giving up (default: 1000)
#   SLEEP_SECS    : seconds to sleep between iterations (default: 0)
#   SHOW_EVERY    : print progress/iteration header every N iterations (default: 100, 1 = every run, <=0 = disabled)
#   LOG_STDERR    : set to 1 to enable verbose stderr logging (HLS_TEST_LOG_STDERR & HLS_TEST_HARNESS_STDERR) (default: 1)
#   NO_BUILD_ONCE : set to non-empty to skip the initial cabal build step
#
# Test selection:
#   TEST_PATTERNS : comma-separated list of entries to run each iteration.
#                   Each entry can be either a plain tasty pattern, or 'BIN::PATTERN' to select a test binary.
#                   Examples:
#                     TEST_PATTERNS='open close'
#                     TEST_PATTERNS='ghcide-tests::open close,func-test::sends indefinite progress notifications'
#                   If set and non-empty, this takes precedence over PATTERN_FILE.
#                   If unset, defaults to 'ghcide-tests::open close' to match prior behavior.
#   PATTERN_FILE  : path to a file with one entry per line.
#                   Lines start with optional 'BIN::', then the tasty pattern. '#' comments and blank lines ignored.
#                   Examples:
#                     ghcide-tests::open close
#                     func-test::sends indefinite progress notifications
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

# Patterns to detect issues
# - Use case-insensitive extended regex for failures/timeouts in logs
# - Broken pipe: case-insensitive fixed-string search
BROKEN_PIPE_RE='Broken pipe'
TEST_FAILED_RE='tests failed|timeout'
DEBUG_DETECT="${DEBUG_DETECT:-0}"

# Resolve what to run each iteration as pairs of BIN and PATTERN
items=() # each item is 'BIN::PATTERN'
if [[ -n "${TEST_PATTERNS:-}" ]]; then
  IFS=',' read -r -a raw_items <<< "${TEST_PATTERNS}"
  for it in "${raw_items[@]}"; do
    # trim
    it="${it#${it%%[![:space:]]*}}"; it="${it%${it##*[![:space:]]}}"
    [[ -z "$it" ]] && continue
    if [[ "$it" == *"::"* ]]; then
      items+=("$it")
    else
      items+=("ghcide-tests::${it}")
    fi
  done
elif [[ -n "${PATTERN_FILE:-}" && -r "${PATTERN_FILE}" ]]; then
  while IFS= read -r line; do
    # trim whitespace, skip comments and blank lines
    trimmed="${line#${line%%[![:space:]]*}}"; trimmed="${trimmed%${trimmed##*[![:space:]]}}"
    [[ -z "${trimmed}" || "${trimmed}" =~ ^[[:space:]]*# ]] && continue
    if [[ "${trimmed}" == *"::"* ]]; then
      items+=("${trimmed}")
    else
      items+=("ghcide-tests::${trimmed}")
    fi
  done < "${PATTERN_FILE}"
else
  # default to the original single test
  items+=("ghcide-tests::open close")
fi

if [[ ${#items[@]} -eq 0 ]]; then
  echo "[loop][error] No test entries provided (via PATTERN_FILE or TEST_PATTERNS)." >&2
  exit 2
fi

# Build required test binaries once upfront (unless NO_BUILD_ONCE is set)
if [[ -z "${NO_BUILD_ONCE:-}" ]]; then
  # collect unique BIN names
  declare -a bins_to_build=()
  for it in "${items[@]}"; do
    bin="${it%%::*}"; seen=0
    if (( ${#bins_to_build[@]} > 0 )); then
      for b in "${bins_to_build[@]}"; do [[ "$b" == "$bin" ]] && seen=1 && break; done
    fi
    [[ $seen -eq 0 ]] && bins_to_build+=("$bin")
  done
  if (( ${#bins_to_build[@]} > 0 )); then
    echo "[loop] Building test targets once upfront: ${bins_to_build[*]}" >&2
    if ! cabal build "${bins_to_build[@]}" >&2; then
      echo "[loop][error] Build failed. Cannot proceed with tests." >&2
      exit 2
    fi
    echo "[loop] Build succeeded. Proceeding with tests." >&2
  fi
fi

# Resolve binary path by name (cache results)
BIN_NAMES=()
BIN_PATHS=()
get_bin_path() {
  local name="$1"
  local i
  for ((i=0; i<${#BIN_NAMES[@]}; i++)); do
    if [[ "${BIN_NAMES[i]}" == "$name" ]]; then
      echo "${BIN_PATHS[i]}"; return
    fi
  done
  local path=""
  path=$(find dist-newstyle -type f -name "$name" -perm -111 2>/dev/null | head -n1 || true)
  BIN_NAMES+=("$name"); BIN_PATHS+=("$path")
  echo "$path"
}

while true; do
  iter=$((iter+1))
  ts=$(date -Iseconds)
  file_num=$((iter % 2))

  # Run each selected item (BIN::PATTERN) in this iteration
  for item in "${items[@]}"; do
    bin_name="${item%%::*}"
    pattern="${item#*::}"
    # sanitize pattern for a log slug
    slug=$(printf '%s' "${bin_name}-${pattern}" | tr -cs 'A-Za-z0-9._-' '-' | sed -E 's/^-+|-+$//g')
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
  "$(get_bin_path "${bin_name}")" +RTS -l -olhlint.eventlog -RTS >"${log}" 2>&1
    set -e

  if grep -aFiq -- "${BROKEN_PIPE_RE}" "${log}"; then
      echo "[loop] Broken pipe reproduced in iteration ${iter} for pattern '${pattern}'. Stopping." | tee -a "${log}" >&2
      echo "[loop] Log file: ${log} (abs: $(pwd)/${log})" | tee -a "${log}" >&2
      echo "[loop] --- Tail (last 60 lines) ---" >&2
      tail -n 60 "${log}" >&2
      exit 1
  elif grep -aEq -- "${TEST_FAILED_RE}" "${log}"; then
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
