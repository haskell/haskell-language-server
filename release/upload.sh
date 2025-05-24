#!/usr/bin/env bash

set -e

# This is a script for preparing and uploading a release of Haskell Language Server.
# Adapted from https://gitlab.haskell.org/bgamari/ghc-utils/-/commits/master/rel-eng/upload.sh
#
# Usage,
#   1. Set $SIGNING_KEY to your key id (prefixed with '=')
#   2. Create a directory called haskell-langauge-server-<release_number> and place the binary tarballs there
#   4. Run this script from that directory
#
# You can also invoke the script with an argument to perform only
# a subset of the usual release,
#
#   upload.sh gen_hashes             generate signed hashes of the release
#                                    tarballs
#   upload.sh sign                   generate signed hashes of the release
#                                    tarballs
#   upload.sh upload                 upload the tarballs and documentation
#                                    to downloads.haskell.org
#
# Prerequisites: moreutils

# Infer release name from directory name
if [ -z "$rel_name" ]; then
    rel_name="$(basename $(pwd))"
fi

# Infer version from tarball names
if [ -z "$ver" ]; then
    ver="$(ls haskell-language-server-*.tar.* | sed -ne 's/haskell-language-server-\([0-9]\+\.[0-9]\+\.[0-9]\+\(\.[0-9]\+\)\?\).\+/\1/p' | head -n1)"
    if [ -z "$ver" ]; then echo "Failed to infer \$ver"; exit 1; fi
fi

echo HLS version $ver

host="gitlab.haskell.org:2222"

usage() {
    echo "Usage: [rel_name=<name>] SIGNING_KEY=<key> $0 <action>"
    echo
    echo "where,"
    echo "  rel_name           gives the release name (e.g. 1.7.0.0)"
    echo "and <action> is one of,"
    echo "  [nothing]          do everything below"
    echo "  gen_hashes         generated hashes of the release tarballs"
    echo "  sign               sign hashes of the release tarballs"
    echo "  upload             upload the tarballs and documentation to downloads.haskell.org"
    echo "  purge_all          purge entire release from the CDN"
    echo "  purge_file file    purge a given file from the CDN"
    echo "  verify             verify the signatures in this directory"
    echo
}

if [ -z "$ver" ]; then
    usage
    exit 1
fi
if [ -z "$rel_name" ]; then
    rel_name="$ver"
fi

# returns the set of files that must have hashes generated.
function hash_files() {
    echo $(find -maxdepth 1 \
         -iname '*.xz' \
      -o -iname '*.gz' \
      -o -iname '*.lz' \
      -o -iname '*.zip' \
    )
    echo $(find -maxdepth 1 -iname '*.patch')
}

function gen_hashes() {
    echo -n "Hashing..."
    sha1sum $(hash_files) >| SHA1SUMS &
    sha256sum $(hash_files) >| SHA256SUMS &
    wait
    echo "done"
}

function sign() {
    # Kill DISPLAY lest pinentry won't work
    DISPLAY=
    eval "$(gpg-agent --daemon --sh --pinentry-program $(which pinentry))"
    for i in $(hash_files) SHA1SUMS SHA256SUMS; do
        if [ -e $i -a -e $i.sig -a $i.sig -nt $i ]; then
            echo "Skipping signing of $i"
            continue
        elif [ -e $i.sig ] && gpg2 --verify $i.sig; then
            # Don't resign if current signature is valid
            touch $i.sig
            continue
        fi
        echo "Signing $i"
        rm -f $i.sig
        gpg2 --use-agent --detach-sign --local-user="$SIGNING_KEY" $i
    done
}

function verify() {
    if [ $(find -iname '*.sig' | wc -l) -eq 0 ]; then
        echo "No signatures to verify"
        return
    fi

    for i in *.sig; do
        echo
        echo Verifying $i
        gpg2 --verify $i $(basename $i .sig)
    done
}

function upload() {
    verify
    chmod ugo+r,o-w -R .
    dir=$(echo $rel_name | sed s/-release//)
    lftp -c " \
	    open -u hls-downloads: sftp://$host && \
	    mirror -P20 -c --reverse --exclude=fetch-gitlab --exclude=out . hls/$dir && \
	    wait all;"
    chmod ugo-w $(ls *.xz *.gz *.zip)
}

function purge_all() {
    # Purge CDN cache
    curl -X PURGE http://downloads.haskell.org/hls/
    curl -X PURGE http://downloads.haskell.org/~hls/
    curl -X PURGE http://downloads.haskell.org/hls/$dir
    curl -X PURGE http://downloads.haskell.org/hls/$dir/
    curl -X PURGE http://downloads.haskell.org/~hls/$dir
    curl -X PURGE http://downloads.haskell.org/~hls/$dir/
    for i in *; do
        purge_file $i
    done
}

function purge_file() {
    curl -X PURGE http://downloads.haskell.org/~hls/$rel_name/$i
    curl -X PURGE http://downloads.haskell.org/~hls/$rel_name/$i/
    curl -X PURGE http://downloads.haskell.org/hls/$rel_name/$i
    curl -X PURGE http://downloads.haskell.org/hls/$rel_name/$i/
}


if [ "x$1" == "x" ]; then
    gen_hashes
    sign
    upload
    purge_all
else
    $@
fi
