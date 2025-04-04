#!/bin/bash

set -eu
set -o pipefail

RELEASE=$1

cd "gh-release-artifacts/haskell-language-server-${RELEASE}"

cat <<EOF > /dev/stdout
    $RELEASE:
      viTags:
        - Latest
      viChangeLog: https://github.com/haskell/haskell-language-server/blob/master/ChangeLog.md
      viPostInstall: *hls-post-install
      viSourceDL:
        dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-src.tar.gz
        dlSubdir: haskell-language-server-$RELEASE
        dlHash: $(sha256sum "haskell-language-server-$RELEASE-src.tar.gz" | awk '{ print $1 }')
      viArch:
        A_64:
          Linux_Debian:
            '< 10': &hls-${RELEASE//./}-64-deb9
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-deb9.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-deb9.tar.xz" | awk '{ print $1 }')
            '(>= 10 && < 11)': &hls-${RELEASE//./}-64-deb10
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-deb10.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-deb10.tar.xz" | awk '{ print $1 }')
            unknown_versioning: &hls-${RELEASE//./}-64-deb11
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-deb11.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-deb11.tar.xz" | awk '{ print $1 }')
          Linux_Ubuntu:
            '( >= 16 && < 19 )': &hls-${RELEASE//./}-64-ubuntu18
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-ubuntu1804.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-ubuntu1804.tar.xz" | awk '{ print $1 }')
            '( >= 20 && < 22 )': &hls-${RELEASE//./}-64-ubuntu20
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-ubuntu2004.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-ubuntu2004.tar.xz" | awk '{ print $1 }')
            unknown_versioning: &hls-${RELEASE//./}-64-ubuntu22
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-ubuntu2204.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-ubuntu2204.tar.xz" | awk '{ print $1 }')
          Linux_Mint:
            '< 20':
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-mint193.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-mint193.tar.xz" | awk '{ print $1 }')
            '(>= 20 && < 21)':
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-mint202.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-mint202.tar.xz" | awk '{ print $1 }')
            '>= 21': *hls-${RELEASE//./}-64-ubuntu22
          Linux_Fedora:
            '< 33': &hls-${RELEASE//./}-64-fedora27
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-fedora27.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-fedora27.tar.xz" | awk '{ print $1 }')
            '>= 33': &hls-${RELEASE//./}-64-fedora33
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-fedora33.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-fedora33.tar.xz" | awk '{ print $1 }')
            unknown_versioning: *hls-${RELEASE//./}-64-fedora27
          Linux_CentOS:
            '( >= 7 && < 8 )': &hls-${RELEASE//./}-64-centos
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-centos7.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-centos7.tar.xz" | awk '{ print $1 }')
            unknown_versioning: *hls-${RELEASE//./}-64-centos
          Linux_RedHat:
            unknown_versioning: *hls-${RELEASE//./}-64-centos
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-linux-unknown.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-linux-unknown.tar.xz" | awk '{ print $1 }')
          Darwin:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-apple-darwin.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-apple-darwin.tar.xz" | awk '{ print $1 }')
          Windows:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-mingw64.zip
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-mingw64.zip" | awk '{ print $1 }')
          FreeBSD:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-x86_64-freebsd.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-x86_64-freebsd.tar.xz" | awk '{ print $1 }')
        A_ARM64:
          Linux_UnknownLinux:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-aarch64-linux-ubuntu2004.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-aarch64-linux-ubuntu2004.tar.xz" | awk '{ print $1 }')
          Darwin:
            unknown_versioning:
              dlUri: https://downloads.haskell.org/~hls/haskell-language-server-$RELEASE/haskell-language-server-$RELEASE-aarch64-apple-darwin.tar.xz
              dlSubdir: haskell-language-server-$RELEASE
              dlHash: $(sha256sum "haskell-language-server-$RELEASE-aarch64-apple-darwin.tar.xz" | awk '{ print $1 }')
EOF

