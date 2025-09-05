#!/usr/bin/env fish

# Dump a GHC RTS .eventlog to a plain-text file using the ghc-events CLI.
# Usage:
#   scripts/eventlog-dump.fish <file.eventlog> [output.txt] [starts_with_prefix] [contains_substring]
#
# Notes:
# - Attempts to find ghc-events in PATH, ~/.cabal/bin, or ~/.local/bin.
# - If not found, will try: cabal install ghc-events
# - Output defaults to <basename>.events.txt in the current directory.

function usage
  echo "Usage: (basename (status filename)) <file.eventlog> [output.txt] [starts_with_prefix] [contains_substring]"
  exit 2
end

if test (count $argv) -lt 1
  usage
end

set evlog $argv[1]
if not test -f $evlog
  echo "error: file not found: $evlog" >&2
  exit 1
end

if test (count $argv) -ge 2
  set out $argv[2]
else
  set base (basename $evlog)
  if string match -q '*\.eventlog' $base
    set out (string replace -r '\\.eventlog$' '.events.txt' -- $base)
  else
    set out "$base.events.txt"
  end
end

# Optional prefix filter: only keep lines that start with this string
set filter_prefix ""
if test (count $argv) -ge 3
  set filter_prefix $argv[3]
end
# Optional contains filter: only keep lines that contain any of the substrings (pipe-separated)
set filter_contains ""
set filter_contains_list
if test (count $argv) -ge 4
  set filter_contains $argv[4]
  set filter_contains_list (string split '|' -- $filter_contains)
end

function find_ghc_events --description "echo absolute path to ghc-events or empty"
  if command -sq ghc-events
    command -s ghc-events
    return 0
  end
  if test -x ~/.cabal/bin/ghc-events
    echo ~/.cabal/bin/ghc-events
    return 0
  end
  if test -x ~/.local/bin/ghc-events
    echo ~/.local/bin/ghc-events
    return 0
  end
  return 1
end

set ghc_events_bin (find_ghc_events)

if test -z "$ghc_events_bin"
  echo "ghc-events not found; attempting to install via 'cabal install ghc-events'..." >&2
  if not command -sq cabal
    echo "error: cabal not found; please install ghc-events manually (e.g., via cabal)." >&2
    exit 1
  end
  cabal install ghc-events
  set ghc_events_bin (find_ghc_events)
  if test -z "$ghc_events_bin"
    echo "error: ghc-events still not found after installation." >&2
    exit 1
  end
end

echo "Dumping events from $evlog to $out..."
if test -n "$filter_prefix" -o -n "$filter_contains"
  $ghc_events_bin show $evlog | while read -l line
    set keep 1
    if test -n "$filter_prefix"
      if not string match -q -- "$filter_prefix*" -- $line
        set keep 0
      end
    end
    if test $keep -eq 1 -a (count $filter_contains_list) -gt 0
      set found 0
      for substr in $filter_contains_list
        if string match -q -- "*$substr*" -- $line
          set found 1
          break
        end
      end
      if test $found -eq 0
        set keep 0
      end
    end
    if test $keep -eq 1
      echo $line
    end
  end > $out
else
  $ghc_events_bin show $evlog > $out
end
set exit_code $status

if test $exit_code -ne 0
  echo "error: dump failed with exit code $exit_code" >&2
  exit $exit_code
end

set -l size ""
if command -sq stat
  # macOS stat prints size with -f%z; suppress errors if not supported
  set size (stat -f%z $out 2>/dev/null)
end
if test -z "$size"
  set size (wc -c < $out)
end

echo "Wrote $out ($size bytes)."
