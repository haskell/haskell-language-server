#!/usr/bin/env bash

set -Eeuxo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"

ls -la out/
cd out/

# create tarball/zip
TARBALL_PREFIX="haskell-language-server-$("$CI_PROJECT_DIR/out/haskell-language-server-wrapper" --numeric-version)"
case "${TARBALL_EXT}" in
    zip)
        zip "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}.${TARBALL_EXT}" haskell-language-server-*
		find . -type f ! -name '*.zip' -delete
        ;;
    tar.xz)
        tar caf "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}.${TARBALL_EXT}" haskell-language-server-*
		find . -type f ! -name '*.tar.xz' -delete
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac


