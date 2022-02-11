#!/usr/bin/env bash

set -Eeuxo pipefail

# Install brew locally in the project dir. Packages will also be installed here.
[ -e "$CI_PROJECT_DIR/.brew" ] || git clone --depth=1 https://github.com/Homebrew/brew $CI_PROJECT_DIR/.brew
export PATH="$CI_PROJECT_DIR/.brew/bin:$CI_PROJECT_DIR/.brew/sbin:$PATH"

# make sure to not pollute the machine with temp files etc
mkdir -p $CI_PROJECT_DIR/.brew_cache
export HOMEBREW_CACHE=$CI_PROJECT_DIR/.brew_cache
mkdir -p $CI_PROJECT_DIR/.brew_logs
export HOMEBREW_LOGS=$CI_PROJECT_DIR/.brew_logs
mkdir -p /private/tmp/.brew_tmp
export HOMEBREW_TEMP=/private/tmp/.brew_tmp

# update and install packages
brew update
brew install ${1+"$@"}
