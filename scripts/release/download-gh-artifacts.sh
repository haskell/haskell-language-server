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

[ ! -e "gh-release-artifacts/haskell-language-server-${RELEASE}" ]

mkdir -p "gh-release-artifacts/haskell-language-server-${RELEASE}"

cd "gh-release-artifacts/haskell-language-server-${RELEASE}"

# github
gh release download "$RELEASE"

## We can't do cirrus releases any more, as we build HLS releases with ghcup vanilla binaries.
## Vanilla means "upstream", aka GHC HQ, and GHC HQ does not provide bindists for FreeBSD.
## Until we start using ghcup's mainstream distribution channel, we can't even begin to build
## binaries for FreeBSD. We keep this here for the next generation or when the situation changes.
##
## We don't use ghcup's mainstream distribution channel, as we only provide vanilla binaries
## as requested by the ghcup distribution channel team.
# cirrus
# curl --fail -L -o "haskell-language-server-${RELEASE}-x86_64-freebsd.tar.xz" \
# 	"https://api.cirrus-ci.com/v1/artifact/github/haskell/haskell-language-server/bindist/bindist/out/haskell-language-server-${RELEASE}-x86_64-freebsd.tar.xz?branch=${RELEASE}"


sha256sum haskell-language-server-* > SHA256SUMS
gpg --detach-sign -u "${SIGNER}" SHA256SUMS

## see comment above
# gh release upload "$RELEASE" "haskell-language-server-${RELEASE}-x86_64-freebsd.tar.xz" SHA256SUMS SHA256SUMS.sig
gh release upload "$RELEASE" SHA256SUMS SHA256SUMS.sig
