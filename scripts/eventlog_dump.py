#!/usr/bin/env python3
"""
Dump a GHC RTS .eventlog to a plain-text file using the ghc-events CLI.
Usage:
  scripts/eventlog_dump.py <file.eventlog> [--out output.txt] [--contains SUBSTR1|SUBSTR2]

Behavior mirrors scripts/eventlog-dump.fish: tries to find ghc-events in PATH,
~/.cabal/bin, or ~/.local/bin. If not found and `cabal` exists in PATH, it will run
`cabal install ghc-events` and retry.

Filtering: if --contains is provided it should be a pipe-separated list of
substrings; a line is kept if it contains any of the substrings.

Exit codes:
  0  : success
  >0 : failures from ghc-events or setup errors
"""
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
from typing import Iterable, List, Optional


def find_ghc_events() -> Optional[str]:
    # 1) PATH
    path = shutil.which("ghc-events")
    if path:
        return path
    # 2) common user bins
    cand = os.path.expanduser("~/.cabal/bin/ghc-events")
    if os.path.isfile(cand) and os.access(cand, os.X_OK):
        return cand
    cand = os.path.expanduser("~/.local/bin/ghc-events")
    if os.path.isfile(cand) and os.access(cand, os.X_OK):
        return cand
    return None


def try_install_ghc_events() -> bool:
    if shutil.which("cabal") is None:
        return False
    print("ghc-events not found; attempting to install via 'cabal install ghc-events'...", file=sys.stderr)
    rc = subprocess.run(["cabal", "install", "ghc-events"])  # let cabal print its own output
    return rc.returncode == 0


def stream_and_filter(cmd: List[str], out_path: str, contains: Optional[Iterable[str]]) -> int:
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    assert proc.stdout is not None
    with open(out_path, "w", encoding="utf-8", newline="\n") as fout:
        for line in proc.stdout:
            if contains:
                if any(sub in line for sub in contains):
                    fout.write(line)
            else:
                fout.write(line)
    # wait for process to finish and capture stderr
    _, err = proc.communicate()
    if proc.returncode != 0:
        # write stderr for debugging
        sys.stderr.write(err)
    return proc.returncode


def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description="Dump GHC eventlog to text with optional substring filtering")
    ap.add_argument("eventlog", help=".eventlog file to dump")
    ap.add_argument("--out", "-o", default=None, help="Output text file (default: <basename>.events.txt)")
    ap.add_argument("--contains", "-c", default=None,
                    help="Pipe-separated substrings to keep (e.g. 'foo|bar'). If omitted, keep all lines.")
    return ap.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> int:
    args = parse_args(argv)
    evlog = args.eventlog
    if not os.path.isfile(evlog):
        print(f"error: file not found: {evlog}", file=sys.stderr)
        return 1

    out = args.out
    if out is None:
        base = os.path.basename(evlog)
        if base.endswith(".eventlog"):
            out = base[:-len(".eventlog")] + ".events.txt"
        else:
            out = base + ".events.txt"

    contains_list: Optional[List[str]] = None
    if args.contains:
        contains_list = [s for s in args.contains.split("|") if s != ""]

    ghc_events = find_ghc_events()
    if ghc_events is None:
        if try_install_ghc_events():
            ghc_events = find_ghc_events()
        else:
            print("error: ghc-events not found; please install it (e.g., 'cabal install ghc-events')", file=sys.stderr)
            return 1
    if ghc_events is None:
        print("error: ghc-events still not found after installation.", file=sys.stderr)
        return 1

    cmd = [ghc_events, "show", evlog]
    print(f"Dumping events from {evlog} to {out} using {ghc_events}...", file=sys.stderr)
    rc = stream_and_filter(cmd, out, contains_list)
    if rc != 0:
        print(f"error: dump failed with exit code {rc}", file=sys.stderr)
        return rc

    try:
        size = os.path.getsize(out)
    except Exception:
        size = None
    if size is None:
        print(f"Wrote {out}.")
    else:
        print(f"Wrote {out} ({size} bytes).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
