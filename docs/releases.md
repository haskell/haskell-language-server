# Releases and distributable binaries

Starting with 0.2.1.0 haskell-language-server provides pre-built binaries on
each [GitHub
release](https://github.com/haskell/haskell-language-server/releases). These
binaries are used by the [vscode-hie-server
extension](https://github.com/alanz/vscode-hie-server) to provide automatic
installation for users on VS Code, but they can also be installed manually
when added to the path.

## Making a new release of haskell-language-server

Go to the [GitHub releases
page](https://github.com/haskell/haskell-language-server/releases) for
haskell-language-server and start to create a new release. Choose or create a
tag, fill out the release notes etc., but before you create it
**make sure to check the pre-release checkbox**. This will prevent VS Code
*extension
users from attempting to install this version before the binaries are
created.

Once the release is created the [GitHub Actions
workflow](https://github.com/haskell/haskell-language-server/actions) will be
kicked off and will start creating binaries. They will be gzipped and
uploaded to the release.

It creates a `haskell-language-server-OS-GHC` binary for each platform
(Linux, macOS, Windows) and each GHC version that we currently support, as well
as a `haskell-language-server-wrapper-OS` binary for each platform. Note that
only one wrapper binary is created per platform, and it should be built with the
most recent GHC version.

Once all these binaries are present

## Distributable binaries
In order to compile a hls binary on one machine and have it run on another, you
need to make sure there are **no hardcoded paths or data-files**.

### ghc libdir
One noteable thing which cannot be hardcoded is the **GHC libdir** – this is
a path to `/usr/local/lib/ghc` or something like that, which was previously
baked in at compile-time with ghc-paths. Note that with static binaries we
can no longer use this because the GHC libdir of the GitHub Actions machine
will most almost certainly not exist on the end user's machine.
Therefore, hie-bios provides `getGhcRuntimeLibDir` to obtain this path on the fly
by consulting the cradle.

### Static binaries
We use the word "distributable" here because technically only the Linux builds
are static. They are built by passing `--enable-executable-static` to cabal.
Static binaries don't really exist on macOS, and there are issues with
proprietary code being linked in on Windows. However, the `.dylib`s linked on
macOS are all already provided by the system:

```
$ objdump -macho --dylibs-used  haskell-language-server
haskell-language-server:
	/usr/lib/libncurses.5.4.dylib (compatibility version 5.4.0, current version 5.4.0)
	/usr/lib/libiconv.2.dylib (compatibility version 7.0.0, current version 7.0.0)
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1281.100.1)
	/usr/lib/libcharset.1.dylib (compatibility version 2.0.0, current version 2.0.0)
```

## The GitHub Actions workflow
It just kicks off a matrix of jobs varying across GHC versions and OSs, building
the binaries with Cabal and extracting them from the dist-newstyle directory.
The binaries are built with -O2.

One caveat is that we need to rename the binaries from
haskell-language-server/haskell-language-server-wrapper to hls/hls-wrapper due to
path length limitations on windows. But whenever we upload them to the release,
we make sure to upload them as their full name variant.

### ghcup
Ghcup can install hls binaries, provided that there is a tarfile
called `haskell-language-server-{macOS,Linux}-$HLS_VERSION.gz`
included in the GitHub release. The `tar` job in the workflow file automates the creation of this.

### Windows
Currently building HLS with GHC 8.8.x on Windows is very flakey and so
is not included by default in the GitHub Actions build matrix. Instead
they need to be built and uploaded manually. See [this
PR](https://github.com/haskell/haskell-language-server/issues/276) for
more details
