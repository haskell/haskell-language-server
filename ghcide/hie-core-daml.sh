#!/usr/bin/env bash
# Copyright (c) 2019 The DAML Authors. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail
cd "$(dirname "$0")"/../..
export RULES_HASKELL_EXEC_ROOT=$PWD/
ENV_FILE=$(mktemp)
ARGS_FILE=$(mktemp)
bazel build //compiler/hie-core:hie-core-exe >/dev/null 2>&1
bazel run --define hie_bios_ghci=True //compiler/damlc:damlc@ghci -- "$ENV_FILE" "$ARGS_FILE" >/dev/null 2>&1
source "$ENV_FILE"
export HIE_BIOS_ARGS="$ARGS_FILE"
./bazel-bin/compiler/hie-core/hie-core-exe $@

