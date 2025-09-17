#!/bin/bash

# Test installing HLS bindist and then run
# every HLS-GHC version on a test module.

set -eux

. .github/scripts/env.sh
. .github/scripts/common.sh

test_package="text-2.1.2"
test_module="src/Data/Text.hs"

create_cradle() {
    echo "cradle:" > hie.yaml
    echo "  cabal:" >> hie.yaml
}

# Tests and benchmarks can't be built on some GHC versions, such as GHC 9.10.1 on Windows.
# Disable these packages for now, building bytestring-0.12.1.0 works completely fine.
create_cabal_project() {
    echo "packages: ./" > cabal.project
    echo "" >> cabal.project
    echo "tests: False" >> cabal.project
    echo "benchmarks: False" >> cabal.project

    echo "flags: -simdutf -pure-haskell" >> cabal.project
}

enter_test_package() {
    local tmp_dir
    tmp_dir=$(mktempdir)
    cd "$tmp_dir"
    cabal unpack "${test_package}"
    cd "${test_package}"
}

# For all HLS GHC versions and the wrapper, run 'typecheck'
# over the $test_module
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
                "${hls}" --debug typecheck "${test_module}" || fail "failed to typecheck with HLS for GHC ${bin_noexe/haskell-language-server-/}"

                # After running the test, free up disk space by deleting the unneeded GHC version.
                # Helps us staying beneath the 14GB SSD disk limit.
                ghcup rm ghc "${bin_noexe/haskell-language-server-/}"
            else
                fail "GHCup failed to install GHC ${bin_noexe/haskell-language-server-/}"
            fi
        fi
    done
    # install the recommended GHC version so the wrapper can launch HLS
    ghcup install ghc --set 9.10.3
    "$bindir/haskell-language-server-wrapper${ext}" typecheck "${test_module}" || fail "failed to typecheck with HLS wrapper"
}

uname -a
uname -p
uname
env

# ensure ghcup
install_ghcup
ghcup install ghc --set 9.4.8

(cd .. && ecabal update) # run cabal update outside project dir

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
        create_cabal_project
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
        create_cabal_project
        test_all_hls "$(ghcup whereis bindir)"

        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac


