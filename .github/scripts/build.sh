#!/bin/bash

set -eux

. .github/scripts/prereq.sh
. .github/scripts/common.sh

uname -a
uname -p
uname
pwd
env

# ensure ghcup
if ! command -v ghcup ; then
	install_ghcup
fi

# ensure cabal-cache
download_cabal_cache "$HOME/.local/bin/cabal-cache"


# build
ecabal update

case "$(uname)" in
    MSYS_*|MINGW*)
		for ghc in $(cat bindist/ghcs-Msys) ; do
			GHC_VERSION="${ghc%,*}"
			args=( -O2 -w "ghc-$GHC_VERSION" --project-file cabal.project --disable-profiling --disable-tests --enable-executable-stripping ${ADD_CABAL_ARGS})
			ghcup install ghc "${GHC_VERSION}"
			ghcup set ghc "${GHC_VERSION}"
			"ghc-${GHC_VERSION}" --info
			"ghc" --info
			# Shorten binary names
			sed -i.bak -e 's/haskell-language-server/hls/g' \
				   -e 's/haskell_language_server/hls/g' \
				   haskell-language-server.cabal cabal.project
			sed -i.bak -e 's/Paths_haskell_language_server/Paths_hls/g' \
				   src/**/*.hs exe/*.hs


			# shellcheck disable=SC2068
			build_with_cache ${args[@]} exe:hls exe:hls-wrapper

			mkdir -p "$CI_PROJECT_DIR/out"

			# shellcheck disable=SC2068
			cp "$(cabal list-bin -v0 ${args[@]} exe:hls)" "$CI_PROJECT_DIR/out/haskell-language-server-${GHC_VERSION}"${ext}
			# shellcheck disable=SC2068
			cp "$(cabal list-bin -v0 ${args[@]} exe:hls-wrapper)" "$CI_PROJECT_DIR/out/haskell-language-server-wrapper"${ext}
			ghcup rm ghc "${GHC_VERSION}"
		done
        ;;
	*)
		sed -i.bak -e '/DELETE MARKER FOR CI/,/END DELETE/d' cabal.project # see comment in cabal.project
		emake --version
		emake GHCUP=ghcup CABAL_CACHE_BIN=cabal-cache S3_HOST="${S3_HOST}" S3_KEY="${ARTIFACT}" hls
		emake GHCUP=ghcup bindist
		rm -rf out/*.*.*
        ;;
esac

# create tarball/zip
TARBALL_PREFIX="haskell-language-server"
case "${TARBALL_EXT}" in
    zip)
		HLS_VERSION="$("$CI_PROJECT_DIR/out/haskell-language-server-8.10.7" --numeric-version)"
		cd "$CI_PROJECT_DIR/out/"
        zip "${TARBALL_PREFIX}-${HLS_VERSION}-${ARTIFACT}.zip" haskell-language-server-*
		find . -mindepth 1 -maxdepth 1 \! -name '*.zip' -exec rm -rf '{}' \;
        ;;
    tar.xz)
		emake --version
		HLS_VERSION="$(emake -s -C out/bindist/haskell-language-server-* version)"
		emake TARBALL="${TARBALL_PREFIX}-${HLS_VERSION}-${ARTIFACT}.tar.xz" bindist-tar
		emake GHCUP=ghcup clean-ghcs
		find out -mindepth 1 -maxdepth 1 \! -name '*.tar.xz' -exec rm -rf '{}' \;
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac

