#!/usr/bin/env bash

set -ex

function replaceHlsVersion() {
    # Update all `version:` fields
    sed -ri "s/^version:( +)${1}/version:\1${2}/" ./*.cabal ./**/*.cabal
    # Update all constraints expected to be in the form `== <version>`.
    # We usually don't force an exact version, so this is relatively unambiguous.
    # We could introduce some more ad-hoc parsing, if there is still ambiguity.
    sed -ri "s/== ${1}/== ${2}/" ./*.cabal ./**/*.cabal
}

if [ $# -ne 2 ];
then
    echo "USAGE: ./relase/update_versions.sh <OLD_VERSION> <NEW_VERSION>"
fi

replaceHlsVersion "${1}" "${2}"
