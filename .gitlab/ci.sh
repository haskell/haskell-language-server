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
export BOOTSTRAP_HASKELL_GHC_VERSION="$GHC_VERSION"
export BOOTSTRAP_HASKELL_CABAL_VERSION="$CABAL_INSTALL_VERSION"
export BOOTSTRAP_HASKELL_VERBOSE=1
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=yes

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

if [[ -n "${LOCAL_CABAL_PROJECT-}" ]]; then
    run cp "$LOCAL_CABAL_PROJECT" cabal.project.local
fi

# some alpines need workaround
if ghc --info | grep -q integer-simple ; then
	echo -e 'package blaze-textual\n    flags: +integer-simple' >> cabal.project.local
fi

run cabal v2-install exe:haskell-language-server exe:haskell-language-server-wrapper \
	-O2 \
    -w "ghc-$GHC_VERSION" \
    --installdir="$CI_PROJECT_DIR/out" \
    --install-method=copy \
    --overwrite-policy=always \
    --enable-executable-static \
    --disable-profiling \
    --disable-tests \
    --enable-split-sections \
    --enable-executable-stripping

cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json"
