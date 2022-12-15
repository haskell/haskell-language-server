#!/bin/bash

set -eux

. .github/scripts/prereq.sh
. .github/scripts/common.sh

test_package="bytestring-0.11.1.0"
test_module="Data/ByteString.hs"

create_cradle() {
    echo "cradle:" > hie.yaml
    echo "  cabal:" >> hie.yaml
}

enter_test_package() {
    local tmp_dir
    tmp_dir=$(mktempdir)
    cd "$tmp_dir"
    cabal unpack "${test_package}"
    cd "${test_package}"
}

test_all_hls() {
    local bin
    local bin_noexe
    local bindir
    local hls
    bindir=$1

    for hls in "${bindir}/"haskell-language-server-* ; do
        bin=${hls##*/}
        bin_noexe=${bin/.exe/}
        if ! [[ "${bin_noexe}" =~ "haskell-language-server-wrapper" ]] && ! [[ "${bin_noexe}" =~ "~" ]] ; then
            if ghcup install ghc --set "${bin_noexe/haskell-language-server-/}" ; then
                "${hls}" typecheck "${test_module}" || fail "failed to typecheck with HLS for GHC ${bin_noexe/haskell-language-server-/}"
            else
                fail "GHCup failed to install GHC ${bin_noexe/haskell-language-server-/}"
            fi
        fi
    done
    "$bindir/haskell-language-server-wrapper${ext}" typecheck "${test_module}" || fail "failed to typecheck with HLS wrapper"
}

uname -a
uname -p
uname
pwd
env

# ensure ghcup
if ! command -v ghcup ; then
    install_ghcup
fi

ecabal update

# unpack
TARBALL_PREFIX="haskell-language-server"
mkdir -p "${GHCUP_BIN}"

case "${TARBALL_EXT}" in
    zip)
        cp "$CI_PROJECT_DIR/out/${TARBALL_PREFIX}"-*-"${ARTIFACT}.zip" .
        unzip ./*.zip
        rm ./*.zip
        mv haskell-language-server-* "${GHCUP_BIN}/"

        enter_test_package
        create_cradle
        test_all_hls "$GHCUP_BIN"

        ;;
    tar.xz)
        hls_bin=$(ls "$CI_PROJECT_DIR/out/${TARBALL_PREFIX}"-*-"${ARTIFACT}.tar.xz")
        hls_ver_=${hls_bin#*haskell-language-server-}
        hls_ver=${hls_ver_%-"${ARTIFACT}"*}
        ghcup install hls -u "file://${hls_bin}" "${hls_ver}" --force

        # cleanup from previous dirty runs
        rm -rf "$HOME"/.local/lib/haskell-language-server-* || true

        # print rpaths and libdirs
        case "$(uname -s)" in
            "Darwin"|"darwin")
                otool -l "$(ghcup whereis basedir)/hls/${hls_ver}/lib/haskell-language-server-${hls_ver}/bin/"haskell-language-server-*
                ;;
            "FreeBSD")
                readelf -Ws "$(ghcup whereis basedir)/hls/${hls_ver}/lib/haskell-language-server-${hls_ver}/bin/"haskell-language-server-*
                ;;
            *)
                objdump -x "$(ghcup whereis basedir)/hls/${hls_ver}/lib/haskell-language-server-${hls_ver}/bin/"haskell-language-server-*
                ;;
        esac
        tree "$(ghcup whereis basedir)/hls/${hls_ver}/lib/haskell-language-server-${hls_ver}/bin/"
        tree "$GHCUP_BIN"

        enter_test_package
        create_cradle
        test_all_hls "$(ghcup whereis bindir)"

        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac


