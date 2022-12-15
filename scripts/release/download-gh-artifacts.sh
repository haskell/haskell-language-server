#!/bin/bash

set -eux
set -o pipefail

RELEASE=$1

mkdir -p "gh-release-artifacts/${RELEASE}"
cd "gh-release-artifacts/${RELEASE}"

gh release download $RELEASE
