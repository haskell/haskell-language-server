#!/usr/bin/env bash
set -eou pipefail
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s ghcide/src ghcide/exe ghcide-bench/exe shake-bench/src ghcide/test/exe --with-group=extra --hint=.hlint.yaml
