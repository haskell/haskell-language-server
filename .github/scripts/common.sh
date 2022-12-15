#!/bin/bash

if [ "${RUNNER_OS}" = "Windows" ] ; then
	ext=".exe"
else
	ext=''
fi

# Colors
RED="0;31"
LT_BROWN="1;33"
LT_BLUE="1;34"

ecabal() {
	cabal "$@"
}

sync_from() {
	if [ "${RUNNER_OS}" != "Windows" ] ; then
		cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"
	fi

	cabal-cache sync-from-archive \
		--host-name-override="${S3_HOST}" \
		--host-port-override=443 \
		--host-ssl-override=True \
		--region us-west-2 \
		$([ "${RUNNER_OS}" != "Windows" ] && echo --store-path="$cabal_store_path") \
		--archive-uri "s3://haskell-language-server/${ARTIFACT}"
}

sync_to() {
	if [ "${RUNNER_OS}" != "Windows" ] ; then
		cabal_store_path="$(dirname "$(cabal help user-config | tail -n 1 | xargs)")/store"
	fi

	cabal-cache sync-to-archive \
		--host-name-override="${S3_HOST}" \
		--host-port-override=443 \
		--host-ssl-override=True \
		--region us-west-2 \
		$([ "${RUNNER_OS}" != "Windows" ] && echo --store-path="$cabal_store_path") \
		--archive-uri "s3://haskell-language-server/${ARTIFACT}"
}

sha_sum() {
	if [ "${RUNNER_OS}" = "FreeBSD" ] ; then
		sha256 "$@"
	else
		sha256sum "$@"
	fi

}

git_describe() {
	git config --global --get-all safe.directory | grep '^\*$' || git config --global --add safe.directory "*"
	git describe --always
}

download_cabal_cache() {
	(
	set -e
	dest="$HOME/.local/bin/cabal-cache"
    url=""
	exe=""
	cd /tmp
	case "${RUNNER_OS}" in
		"Linux")
			case "${ARCH}" in
				"32") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.4/i386-linux-cabal-cache-1.0.5.4
					;;
				"64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.4/x86_64-linux-cabal-cache-1.0.5.4
					;;
				"ARM64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.4/aarch64-linux-cabal-cache-1.0.5.4
					;;
				"ARM") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.4/armv7-linux-cabal-cache-1.0.5.4
					;;
			esac
			;;
		"FreeBSD")
			url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.4/x86_64-portbld-freebsd-cabal-cache-1.0.5.4
			;;
		"Windows")
			exe=".exe"
			url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.4/x86_64-mingw64-cabal-cache-1.0.5.4.exe
			;;
		"macOS")
			case "${ARCH}" in
				"ARM64") url=https://downloads.haskell.org/ghcup/unofficial-bindists/cabal-cache/1.0.5.4/aarch64-apple-darwin-cabal-cache-1.0.5.4
					;;
				"64") url=https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal-cache/1.0.5.4/x86_64-apple-darwin-cabal-cache-1.0.5.4
					;;
			esac
			;;
	esac

	if [ -n "${url}" ] ; then
		case "${url##*.}" in
			"gz")
				curl -L -o - "${url}" | gunzip > cabal-cache${exe}
				;;
			*)
				curl -o cabal-cache${exe} -L "${url}"
				;;
		esac
		sha_sum cabal-cache${exe}
		mv "cabal-cache${exe}" "${dest}${exe}"
		chmod +x "${dest}${exe}"
	fi
    )
}

build_with_cache() {
	ecabal configure "$@"
	ecabal build --dependencies-only "$@" --dry-run
	sync_from
	ecabal build --dependencies-only "$@" || sync_to
	sync_to
	ecabal build "$@"
	sync_to
}

install_ghcup() {
	find "$GHCUP_INSTALL_BASE_PREFIX"
	mkdir -p "$GHCUP_BIN"
	mkdir -p "$GHCUP_BIN"/../cache

	if [ "${RUNNER_OS}" = "FreeBSD" ] ; then
		curl -o ghcup https://downloads.haskell.org/ghcup/tmp/x86_64-portbld-freebsd-ghcup-0.1.18.1
		chmod +x ghcup
		mv ghcup "$HOME/.local/bin/ghcup"
	else
		curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_MINIMAL=1 sh
		source "$(dirname "${GHCUP_BIN}")/env"
		ghcup install ghc --set "${BOOTSTRAP_HASKELL_GHC_VERSION}"
		ghcup install cabal --set "${BOOTSTRAP_HASKELL_CABAL_VERSION}"
	fi
}

strip_binary() {
	(
	set -e
	local binary=$1
	case "$(uname -s)" in
		"Darwin"|"darwin")
			;;
		MSYS_*|MINGW*)
			;;
		*)
			strip -s "${binary}"
		   ;;
   esac
	)
}

# GitLab Pipelines log section delimiters
# https://gitlab.com/gitlab-org/gitlab-foss/issues/14664
start_section() {
  name="$1"
  echo -e "section_start:$(date +%s):$name\015\033[0K"
}

end_section() {
  name="$1"
  echo -e "section_end:$(date +%s):$name\015\033[0K"
}

echo_color() {
  local color="$1"
  local msg="$2"
  echo -e "\033[${color}m${msg}\033[0m"
}

error() { echo_color "${RED}" "$1"; }
warn() { echo_color "${LT_BROWN}" "$1"; }
info() { echo_color "${LT_BLUE}" "$1"; }

fail() { error "error: $1"; exit 1; }

run() {
  info "Running $*..."
  "$@" || ( error "$* failed"; return 1; )
}

emake() {
	if command -v gmake >/dev/null 2>&1 ; then
		gmake "$@"
	else
		make "$@"
	fi
}

mktempdir() {
	case "$(uname -s)" in
		"Darwin"|"darwin")
			mktemp -d -t hls_ci.XXXXXXX
			;;
		*)
			mktemp -d
			;;
	esac
}
