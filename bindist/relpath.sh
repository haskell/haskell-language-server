#!/bin/sh

# POSIX shell implementation of `realpath --relative-to=$1 $2.
# This is an adaptation of the implementation from
# <https://github.com/Offirmo/offirmo-shell-lib>.

# returns relative path to $2=$target from $1=$source
## NOTE : path are compared in text only. They don’t have to exist
##        and they WONT be normalized/escaped
## Result in "$return_value"# both $1 and $2 are absolute paths beginning with /

# @FUNCTION: die
# @USAGE: [msg]
# @DESCRIPTION:
# Exits the shell script with status code 2
# and prints the given message in red to STDERR, if any.
die() {
    (>&2 echo "$1")
    exit 2
}

# @FUNCTION: posix_realpath
# @USAGE: <file>
# @DESCRIPTION:
# Portably gets the realpath and prints it to stdout.
# This was initially inspired by
#   https://gist.github.com/tvlooy/cbfbdb111a4ebad8b93e
#   and
#   https://stackoverflow.com/a/246128
#
# If the file does not exist, just prints it appended to the current directory.
# @STDOUT: realpath of the given file
posix_realpath() {
    [ -z "$1" ] && die "Internal error: no argument given to posix_realpath"
    current_loop=0
    max_loops=50
    mysource=$1

    while [ -h "${mysource}" ]; do
        current_loop=$((current_loop+1))
        mydir="$( cd -P "$( dirname "${mysource}" )" > /dev/null 2>&1 && pwd )"
        mysource="$(readlink "${mysource}")"
        [ "${mysource%${mysource#?}}"x != '/x' ] && mysource="${mydir}/${mysource}"

        if [ ${current_loop} -gt ${max_loops} ] ; then
            (>&2 echo "${1}: Too many levels of symbolic links")
			exit 1
        fi
    done
    mydir="$( cd -P "$( dirname "${mysource}" )" > /dev/null 2>&1 && pwd )"

    # TODO: better distinguish between "does not exist" and "permission denied"
    if [ -z "${mydir}" ] ; then
        (>&2 echo "${1}: Permission denied")
		echo "$(pwd)/$1"
    else
        echo "${mydir%/}/$(basename "${mysource}")"
    fi

    unset current_loop max_loops mysource mydir
}


src="$(posix_realpath $1)" || exit 1
target="$(posix_realpath $2)" || exit 1

common_part="$src"
result=""

while test "${target#$common_part}" = "${target}" ; do
    #echo "common_part is now : \"$common_part\""
    #echo "result is now      : \"$result\""
    #echo "target#common_part : \"${target#$common_part}\""
    # no match, means that candidate common part is not correct
    # go up one level (reduce common part)
    common_part="$(dirname "$common_part")"
    # and record that we went back
    if test -z "$result" ; then
        result=".."
    else
        result="../$result"
    fi
done

#echo "common_part is     : \"$common_part\""

if test "$common_part" = "/" ; then
    # special case for root (no common path)
    result="$result/"
fi

# since we now have identified the common part,
# compute the non-common part
forward_part="${target#$common_part}"
#echo "forward_part = \"$forward_part\""

if test -n "$result" && test -n "$forward_part" ; then
    #echo "(simple concat)"
    result="$result$forward_part"
elif test -n "$forward_part" ; then
    #echo "(concat with slash removal)"
    result="$(printf "%s" "$forward_part" | cut -c 1-)"
fi

printf "%s" "$result"
