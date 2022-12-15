#!/bin/sh

case "$(uname -s)" in
	MSYS_*|MINGW*)
		ext=".exe"
		;;
	*)
		ext=""
	   ;;
esac

if [ -n "${CABAL_CACHE_DISABLE}" ] ; then
	echo "cabal-cache disabled (CABAL_CACHE_DISABLE set)"
elif [ -n "${CABAL_CACHE_NONFATAL}" ] ; then
	"cabal-cache${ext}" "$@" || echo "cabal-cache failed (CABAL_CACHE_NONFATAL set)"
else
	exec "cabal-cache${ext}" "$@"
fi

