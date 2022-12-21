#!/usr/bin/env bash

set -Eeuxo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"
source "$CI_PROJECT_DIR/.gitlab/setup.sh"

export GHCUP_INSTALL_BASE_PREFIX="$CI_PROJECT_DIR/toolchain"
export CABAL_DIR="$CI_PROJECT_DIR/cabal"
EXE_EXTENSION=""

case "$(uname)" in
    MSYS_*|MINGW*)
        export CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
        GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/bin"
        EXE_EXTENSION=".exe"
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
export BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=yes

# for some reason the subshell doesn't pick up the arm64 environment on darwin
# and starts installing x86_64 GHC
case "$(uname -s)" in
    "Darwin"|"darwin")
        case "$(/usr/bin/arch)" in
            aarch64|arm64|armv8l)
                curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | arch -arm64 /bin/bash
                export C_INCLUDE_PATH="`xcrun --show-sdk-path`/usr/include/ffi"
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
        # workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/21196
        # export PATH="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin:${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/usr/bin:$PATH"
        # ls ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin
        # cp ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin/libgcc_s_seh-1.dll ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/bin
        # cp ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin/libwinpthread-1.dll ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/bin
        ghc --info
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

        cp "$(cabal list-bin -v0 ${args[@]} exe:hls)" "$CI_PROJECT_DIR/out/haskell-language-server-${GHC_VERSION}"$EXE_EXTENSION
        cp "$(cabal list-bin -v0 ${args[@]} exe:hls-wrapper)" "$CI_PROJECT_DIR/out/haskell-language-server-wrapper"$EXE_EXTENSION
        ;;
    *)
        sed -i.bak -e '/DELETE MARKER FOR CI/,/END DELETE/d' cabal.project # see comment in cabal.project
        emake --version
        emake GHCUP=ghcup hls-ghc
        emake GHCUP=ghcup bindist-ghc
        rm -rf out/*.*.*
        ;;
esac

cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json"

cd "$CI_PROJECT_DIR/out/"
