#!/bin/bash

set -eux

. .github/scripts/env.sh

ls -lah
ls -lah out/
ls -lah store/

tar cvf "out-${ARTIFACT}-${GHC_VERSION}.tar" out/ store/
