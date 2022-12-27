#!/usr/bin/env bash

set -Eeuxo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"
source "$CI_PROJECT_DIR/.gitlab/setup.sh"

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

# case "$(uname)" in
#     MSYS_*|MINGW*)
#         # workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/21196
#         export PATH="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin:${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/usr/bin:$PATH"
#         ls ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin
#         cp ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin/libgcc_s_seh-1.dll ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/bin
#         cp ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin/libwinpthread-1.dll ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/bin
#         ghc --info
#         ;;
# 	*) ;;
# esac

# make sure out/ dir is gone, so build host rpaths don't
# kick in (TODO: we should probably remove those)
mv "$CI_PROJECT_DIR/out"/*.tar.xz .
rm -rf "$CI_PROJECT_DIR/out/"

# cleanup from previous dirty runs
rm -rf "$HOME"/.local/lib/haskell-language-server-* || true

# install
tar xf *.tar.xz
rm *.tar.xz
cd haskell-language-server-*
INSTALL_DIR=$(dirname "${GHCUP_BINDIR}") || exit 1
[ -d "$INSTALL_DIR" ] || exit 1
emake PREFIX="${INSTALL_DIR}" install

# print rpaths and libdirs
case "$(uname -s)" in
	"Darwin"|"darwin")
		otool -l "$INSTALL_DIR"/lib/haskell-language-server-*/bin/haskell-language-server-*
		;;
	*)
		objdump -x "$INSTALL_DIR"/lib/haskell-language-server-*/bin/haskell-language-server-*
		;;
esac
tree "$INSTALL_DIR"/lib/haskell-language-server-*
tree "$INSTALL_DIR"/bin

tmp_dir=$(mktempdir)
cd "$tmp_dir"
cabal unpack bytestring-0.11.1.0
cd bytestring-0.11.1.0
echo "cradle:" > hie.yaml
echo "  cabal:" >> hie.yaml
haskell-language-server-wrapper typecheck Data/ByteString.hs
