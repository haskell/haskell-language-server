#!/usr/bin/env bash
set -eou pipefail
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s . --with-group=extra
