#!/bin/bash

set -eux

. .github/scripts/env.sh
. .github/scripts/common.sh

# ensure ghcup
if ! command -v ghcup ; then
	install_ghcup
fi

# create tarball/zip
case "${TARBALL_EXT}" in
    zip)
		HLS_VERSION="$(grep '^version:' haskell-language-server.cabal | awk '{ print $2 }')"
		(
			cd "$CI_PROJECT_DIR/out/${ARTIFACT}"
			zip "$CI_PROJECT_DIR/out/haskell-language-server-${HLS_VERSION}-${ARTIFACT}.zip" haskell-language-server-*
		)
        ;;
    tar.xz)
		# we need to control the order, so the hls wrapper binary is installed
		# from the oldest version in the list
		: "${GHCS:="$(cd "$CI_PROJECT_DIR/out/${ARTIFACT}" && rm -f ./*.json && for ghc in * ; do printf "%s\n" "$ghc" ; done | sort -r | tr '\n' ' ')"}"
		emake --version
		emake GHCUP=ghcup ARTIFACT="${ARTIFACT}" GHCS="${GHCS}" bindist
		emake GHCUP=ghcup                        GHCS="${GHCS}" clean-ghcs
		emake GHCUP=ghcup ARTIFACT="${ARTIFACT}"                bindist-tar
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac
