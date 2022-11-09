#!/usr/bin/env bash

set -Eeuxo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"
source "$CI_PROJECT_DIR/.gitlab/setup.sh"

ls
ls -la out/
ls -la out/bindist/
cat GNUmakefile

# create tarball/zip
case "${TARBALL_EXT}" in
    zip)
		HLS_VERSION="$("$CI_PROJECT_DIR/out/haskell-language-server-8.10.7" --numeric-version)"
		cd out/
        zip "${TARBALL_PREFIX}-${HLS_VERSION}-${TARBALL_ARCHIVE_SUFFIX}.zip" haskell-language-server-*
		find . -type f ! -name '*.zip' -delete
        ;;
    tar.xz)
		emake --version
		emake bindist
		emake bindist-tar
		rm -rf out/bindist
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac


