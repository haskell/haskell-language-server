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

cd "$CI_PROJECT_DIR/out/"

tar xf *.tar.xz
rm *.tar.xz
cd haskell-language-server-*
emake PREFIX=$HOME/.local
export PATH="$HOME/.local/bin:$PATH"
tmp_dir=$(mktempdir)
cd "$tmp_dir"
cabal unpack bytestring-0.11.1.0
cd bytestring-0.11.1.0
echo "cradle:" > hie.yaml
echo "  cabal:" >> hie.yaml
haskell-language-server-wrapper typecheck Data/ByteString.hs
