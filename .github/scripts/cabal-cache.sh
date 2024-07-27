#!/usr/bin/env bash

case "$(uname -s)" in
	MSYS_*|MINGW*)
		ext=".exe"
		;;
	*)
		ext=""
	   ;;
esac

if [ "${CABAL_CACHE_DISABLE}" = "yes" ] ; then
	echo "cabal-cache disabled (CABAL_CACHE_DISABLE set)"
elif [ "${CABAL_CACHE_NONFATAL}" = "yes" ] ; then
	time "cabal-cache${ext}" "$@" || echo "cabal-cache failed (CABAL_CACHE_NONFATAL set)"
else
	time "cabal-cache${ext}" "$@"
	exit $?
fi

