#!/usr/bin/env bash

set -Eeuxo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"

export GHCUP_INSTALL_BASE_PREFIX="$CI_PROJECT_DIR/toolchain"
export CABAL_DIR="$CI_PROJECT_DIR/cabal"

case "$(uname)" in
    MSYS_*|MINGW*)
        export CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
		GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/bin"
        ;;
	*)
		GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin"
		;;
esac

mkdir -p "$CABAL_DIR"
mkdir -p "$GHCUP_BINDIR"
export PATH="$GHCUP_BINDIR:$PATH"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION="${GHC_VERSION:-recommended}"
export BOOTSTRAP_HASKELL_CABAL_VERSION="$CABAL_INSTALL_VERSION"
export BOOTSTRAP_HASKELL_VERBOSE=1
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=yes

# for some reason the subshell doesn't pick up the arm64 environment on darwin
# and starts installing x86_64 GHC
case "$(uname -s)" in
	"Darwin"|"darwin")
		case "$(/usr/bin/arch)" in
			aarch64|arm64|armv8l)
				curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | arch -arm64 /bin/bash
				;;
			*)
				curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
				;;
		esac
		;;
	*)
		curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
		;;
esac

case "$(uname)" in
    MSYS_*|MINGW*)
		# Shorten binary names
		sed -i.bak -e 's/haskell-language-server/hls/g' \
			   -e 's/haskell_language_server/hls/g' \
			   haskell-language-server.cabal $CABAL_PROJECT
		sed -i.bak -e 's/Paths_haskell_language_server/Paths_hls/g' \
			   src/**/*.hs exe/*.hs

		args=(
			-O2
			-w "ghc-$GHC_VERSION"
			--project-file "$CABAL_PROJECT"
			--disable-profiling
			--disable-tests
			--enable-executable-stripping
			${ADD_CABAL_ARGS}
		)

		run cabal v2-build ${args[@]} exe:hls exe:hls-wrapper

		mkdir "$CI_PROJECT_DIR/out"

		cp "$(cabal list-bin ${args[@]} exe:hls)" "$CI_PROJECT_DIR/out/haskell-language-server-${GHC_VERSION}"
		cp "$(cabal list-bin ${args[@]} exe:hls-wrapper)" "$CI_PROJECT_DIR/out/haskell-language-server-wrapper"
        ;;
	*)
		emake --version
		emake GHCUP=ghcup hls
		emake GHCUP=ghcup bindist
		rm -rf out/*.*.*
        ;;
esac

cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json"

cd "$CI_PROJECT_DIR/out/"
