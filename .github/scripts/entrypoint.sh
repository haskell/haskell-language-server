#!/bin/bash

set -x

bash -c "$INSTALL curl bash git tree $TOOLS"

unset INSTALL
unset TOOLS

if [ "${ARTIFACT}" = "x86_64-linux-unknown" ]; then
  echo "NAME=Linux" > /etc/os-release
  echo "ID=linux" >> /etc/os-release
  echo "PRETTY_NAME=Linux" >> /etc/os-release
fi

case "$STAGE" in
  "BUILD")
    bash .github/scripts/build.sh
    tar cf out-${ARTIFACT}-${GHC_VERSION}.tar out/ store/
    ;;
  "BINDIST")
    set -eux
    for bindist in out-*.tar ; do
        tar -xf "${bindist}"
    done
    unset bindist
    bash .github/scripts/bindist.sh
    ;;
  "TEST")
    bash .github/scripts/test.sh
esac

