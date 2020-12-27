#!/usr/bin/env bash
set -eou pipefail
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s src exe bench shake-bench/src test/exe --with-group=extra
