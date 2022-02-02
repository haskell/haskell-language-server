#!/usr/bin/env bash

set -Eeuxo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"

ls -la out/

# create tarball/zip
TARBALL_PREFIX="haskell-language-server"
case "${TARBALL_EXT}" in
    zip)
		HLS_VERSION="$("$CI_PROJECT_DIR/out/haskell-language-server-wrapper" --numeric-version)"
		cd out/
        zip "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}-${HLS_VERSION}.zip" haskell-language-server-*
		find . -type f ! -name '*.zip' -delete
        ;;
    tar.xz)
		emake TARBALL="${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}-${HLS_VERSION}.tar.xz" bindist-tar
		rm -rf out/bindist
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac


