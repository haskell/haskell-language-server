#!/bin/bash

set -eux
set -o pipefail

RELEASE=$1
TAG=v$RELEASE

mkdir -p "gh-release-artifacts/${RELEASE}"
cd "gh-release-artifacts/${RELEASE}"

gh release download v$RELEASE
