#!/bin/sh

set -eux

. .github/scripts/env.sh

if [ -e "$HOME/.brew" ] ; then
	(
	cd "$HOME/.brew"
	git fetch --depth 1
	git reset --hard origin/master
	)
else
	git clone --depth=1 https://github.com/Homebrew/brew "$HOME/.brew"
fi
export PATH="$HOME/.brew/bin:$HOME/.brew/sbin:$PATH"

mkdir -p $CI_PROJECT_DIR/.brew_cache
export HOMEBREW_CACHE=$CI_PROJECT_DIR/.brew_cache
mkdir -p $CI_PROJECT_DIR/.brew_logs
export HOMEBREW_LOGS=$CI_PROJECT_DIR/.brew_logs
export HOMEBREW_TEMP=$(mktemp -d)

#brew update
brew install ${1+"$@"}
