#!/bin/bash

mkdir -p "$HOME"/.local/bin

export PATH="$HOME/.local/bin:$PATH"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION="${GHC_VER:-recommended}"
export BOOTSTRAP_HASKELL_CABAL_VERSION="${CABAL_VER:-recommended}"
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=yes
export BOOTSTRAP_HASKELL_INSTALL_NO_STACK=yes

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

if [ "${RUNNER_OS}" = "macOS" ] ; then
	if ! command -v brew ; then
		[ -e "$HOME/.brew" ] ||
			git clone --depth=1 https://github.com/Homebrew/brew "$HOME/.brew"
		export PATH="$HOME/.brew/bin:$HOME/.brew/sbin:$PATH"
		brew update
	fi
	if ! command -v git ; then
		brew install git
	fi
	if ! command -v realpath ; then
		brew install coreutils
	fi

	brew install autoconf automake make tree
    if [ "${ARCH}" = "ARM64" ] ; then
		brew install llvm@13
		export PATH="$HOME/.brew/opt/llvm@13/bin:$PATH"
		export CC="$HOME/.brew/opt/llvm@13/bin/clang"
		export CXX="$HOME/.brew/opt/llvm@13/bin/clang++"
		export LD=ld
		export AR="$HOME/.brew/opt/llvm@13/bin/llvm-ar"
		export RANLIB="$HOME/.brew/opt/llvm@13/bin/llvm-ranlib"
	fi
fi

