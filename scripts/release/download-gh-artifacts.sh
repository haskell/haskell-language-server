#!/bin/bash

set -eu
set -o pipefail

RELEASE=$1
SIGNER=$2

echo "RELEASE: $RELEASE"
echo "SIGNER: $SIGNER"

for com in gh gpg curl sha256sum ; do
	command -V ${com} >/dev/null 2>&1
done

[ ! -e "gh-release-artifacts/${RELEASE}" ]

mkdir -p "gh-release-artifacts/${RELEASE}"
cd "gh-release-artifacts/${RELEASE}"

# github
gh release download "$RELEASE"

# cirrus
curl --fail -L -o "haskell-language-server-${RELEASE}-x86_64-freebsd.tar.xz" \
	"https://api.cirrus-ci.com/v1/artifact/github/haskell/haskell-language-server/build/binaries/out/haskell-language-server-${RELEASE}-x86_64-freebsd.tar.xz?branch=${RELEASE}"


sha256sum haskell-language-server-* > SHA256SUMS
gpg --detach-sign -u "${SIGNER}" SHA256SUMS

gh release upload "$RELEASE" "haskell-language-server-${RELEASE}-x86_64-freebsd.tar.xz" SHA256SUMS SHA256SUMS.sig
