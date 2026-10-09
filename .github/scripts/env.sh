#!/bin/bash

mkdir -p "$HOME"/.local/bin

if [ "${RUNNER_OS}" = "Windows" ] ; then
	ext=".exe"
else
	ext=''
fi

export PATH="$HOME/.local/bin:$PATH"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_CABAL_VERSION="${CABAL_VER:-3.16.1.0}"
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=no
export BOOTSTRAP_HASKELL_INSTALL_NO_STACK=yes
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1

if [ "${RUNNER_OS}" = "Windows" ] ; then
    # on windows use pwd to get unix style path
    CI_PROJECT_DIR="$(pwd)"
    export CI_PROJECT_DIR
    export GHCUP_INSTALL_BASE_PREFIX="/c"
    export GHCUP_BIN="$GHCUP_INSTALL_BASE_PREFIX/ghcup/bin"
    export PATH="$GHCUP_BIN:$PATH"
    export CABAL_DIR="C:\\Users\\runneradmin\\AppData\\Roaming\\cabal"
else
    export CI_PROJECT_DIR="${GITHUB_WORKSPACE}"
    export GHCUP_INSTALL_BASE_PREFIX="$CI_PROJECT_DIR"
    export GHCUP_BIN="$GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin"
    export PATH="$GHCUP_BIN:$PATH"
    export CABAL_DIR="$CI_PROJECT_DIR/cabal"
    export CABAL_CACHE="$CI_PROJECT_DIR/cabal-cache"
fi

export DEBIAN_FRONTEND=noninteractive
export TZ=Asia/Singapore
export LANG=en_US.UTF-8
export LC_ALL=C.UTF-8
