#!/bin/bash

set -eux

. .github/scripts/env.sh
. .github/scripts/common.sh

uname -a
uname -p
uname
pwd
env

# Ensure ghcup is present and properly configured.
# Sets up the vanilla channel, as HLS CI provides binaries
# for GHCup's vanilla channel.
install_ghcup

# ensure cabal-cache
download_cabal_cache "$HOME/.local/bin/cabal-cache"


# build
ghcup install ghc "${GHC_VERSION}" || fail_with_ghcup_logs "install ghc"
ghcup set ghc "${GHC_VERSION}"
sed -i.bak -e '/DELETE MARKER FOR CI/,/END DELETE/d' cabal.project # see comment in cabal.project
ecabal --version
ecabal update
ecabal user-config diff
ecabal user-config init -f
"ghc-${GHC_VERSION}" --info
"ghc" --info

mkdir -p "$CI_PROJECT_DIR/out/${ARTIFACT}"
mkdir -p "$CI_PROJECT_DIR/out/plan.json"

case "$(uname)" in
    MSYS_*|MINGW*)
    # cat "C:\Users\runneradmin\AppData\Roaming\cabal\config"
    # sed -ic "/extra-include-dirs/d" "C:\Users\runneradmin\AppData\Roaming\cabal\config"
    # sed -ic "/extra-lib-dirs/d" "C:\Users\runneradmin\AppData\Roaming\cabal\config"
    cat "C:\Users\runneradmin\AppData\Roaming\cabal\config"
		args=( -O2 -w "ghc-$GHC_VERSION" --project-file cabal.project --disable-profiling --disable-tests --enable-executable-stripping ${ADD_CABAL_ARGS})

		# Shorten binary names
		# due to MAX_PATH issues on windows
		sed -i.bak -e 's/haskell-language-server/hls/g' \
			   -e 's/haskell_language_server/hls/g' \
			   haskell-language-server.cabal cabal.project
		sed -i.bak -e 's/Paths_haskell_language_server/Paths_hls/g' \
			   src/**/*.hs exe/*.hs

		# shellcheck disable=SC2068
		build_with_cache ${args[@]} exe:hls exe:hls-wrapper
		cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json/${ARTIFACT}-ghc-${GHC_VERSION}-plan.json"

		# shellcheck disable=SC2068
		cp "$(cabal list-bin -v0 ${args[@]} exe:hls)" "$CI_PROJECT_DIR/out/${ARTIFACT}/haskell-language-server-${GHC_VERSION}${ext}"
		# shellcheck disable=SC2068
		cp "$(cabal list-bin -v0 ${args[@]} exe:hls-wrapper)" "$CI_PROJECT_DIR/out/${ARTIFACT}/haskell-language-server-wrapper${ext}"
        ;;
	*)
		emake --version
		emake GHCUP=ghcup CABAL_CACHE_BIN=cabal-cache.sh S3_HOST="${S3_HOST}" S3_KEY="${ARTIFACT}" GHC_VERSION="${GHC_VERSION}" hls-ghc
        ;;
esac


