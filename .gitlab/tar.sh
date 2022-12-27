#!/usr/bin/env bash

set -Eeuxo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"
source "$CI_PROJECT_DIR/.gitlab/setup.sh"


# create tarball/zip
case "${TARBALL_EXT}" in
    zip)
    TARBALL_PREFIX="haskell-language-server"
		HLS_VERSION="$("$CI_PROJECT_DIR/out/haskell-language-server-8.10.7" --numeric-version)"
		cd out/
        zip "${TARBALL_PREFIX}-${HLS_VERSION}-${TARBALL_ARCHIVE_SUFFIX}.zip" haskell-language-server-*
		find . -type f ! -name '*.zip' -delete
        ;;
    tar.xz)
		ls -la out/
		ls -la out/bindist/
		ls -la out/bindist/*/
		emake --version
		emake bindist
		emake bindist-tar
		rm -rf out/bindist
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac


