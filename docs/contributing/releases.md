# Releases and distributable binaries

Starting with 0.2.1.0 haskell-language-server provides pre-built binaries on
each [GitHub
release](https://github.com/haskell/haskell-language-server/releases). These
binaries are used by the [vscode-hie-server
extension](https://github.com/alanz/vscode-hie-server) to provide automatic
installation for users on VS Code, but they can also be installed manually
when added to the path.

Starting with 0.8.0.0 haskell-language-server and all its related packages
(core libraries like ghcide, plugins and hls itself) is being released in
[hackage](https://hackage.haskell.org/package/haskell-language-server) as well.
This allow cabal users to install it with `cabal install haskell-language-server`
and it is being used in nix environments.

## Minimal checklist

### github release

* [ ] generate the list of pull requests finished since the last release using the [haskell script](https://github.com/haskell/haskell-language-server/blob/master/GenChangelogs.hs) in the project root.
  Nix users should run command `gen-hls-changelogs` (a wrapper of the script) in nix-shell instead.
* [ ] add that list to the actual [Changelog](https://github.com/haskell/haskell-language-server/blob/master/ChangeLog.md) with a description of the release.
* [ ] bump up versions of changed packages. All are optional but [haskell-language-server itself](https://github.com/haskell/haskell-language-server/blob/master/haskell-language-server.cabal).
* [ ] create the tag and make an initial prerelease to trigger the ci workflow (see details below)
* [ ] contact ghcup team (#haskell-ghcup irc channel or via its [repo](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues)) to try to sync our release and its inclusion in ghcup
* [ ] check uploaded binaries (see windows note below) and the release description (usually the changelog entry) and uncheck the prerelease box
* [ ] make public the release in the usual social channels: irc, twitter, reddit, discord, discourse, mailing lists, etc (not required but useful to spread the word :slightly_smiling_face:)

### hackage release

* [ ] bump up package versions following the [pvp specification](https://pvp.haskell.org/) if they are not already updated
* [ ] create ${version}-hackage branch to trigger the hackage github workflow which will upload all changed packages to hackage as candidates
* [ ] check manually candidates in hackage
* [ ] publish them definitely

## Making a new release of haskell-language-server in github

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

It creates a `haskell-language-server-${os}-${ghcVersion}` binary for each platform
(Linux, macOS, Windows) and each GHC version that we currently support, as well
as a `haskell-language-server-wrapper-${os}` binary for each platform. Note that
only one wrapper binary is created per platform, and it should be built with the
most recent GHC version.

### ghcup

It creates a `haskell-language-server-${os}-${hlsVersion}.tar.gz` tarball with
the binaries for *all* supported ghc versions and a custom source tarball to help
downstream publishers in the distribution of the release.

The most prominent publisher using them is `ghcup`.

### checksums

The sha256 checksum of all artifacts are listed in the `SHA256SUMS` release file.

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

```bash
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

### Failing workflow

If the workflow fail and all of some binaries has not been uploaded,
the prerelease and the tag itself has to be recreated to start it again.
If only some of the artefacts are missing, an alternative could be make
the release in a fork and upload manually them.

If they are missing due to ci specific problems we can build the executable locally
and add it to the existing release.

### Updating release artifacts

*IMPORTANT: release artifacts must not be modified, cause it would break
its secure distribution using their hashes. We should only add new ones.*

To manually upload a new binary we should:

* Add the new tar/zip following the name conventions of existing ones
  * `haskell-language-server-${os}-${ghcVersion}.gz` for `Linux` and `macOS` and `haskell-language-server-Windows-${ghcVersion}.exe.zip` for `Windows`
  * the binary inside the gz file is named `haskell-language-server-${ghcVersion}` (with the `.exe` extension for `Windows`). Note that the binary name does not contain the `${os}` part.
* Add the executable to the existing tar `haskell-language-server-${os}-${ghcVersion}.tar.gz` *locally* and upload it under a new name `haskell-language-server-${os}-${ghcVersion}-rev${n}.tar.gz` following the same schema for the binary as the previous one.
  * `-rev${n}` is the next revision number of the tarball, starting at 1.
  * we should contact users of the tarball (particularly ghcup) to notify the change

## Hackage release workflow

We aim to do hackage releases following the github ones described above.
To help in that job we have added a [github workflow](https://github.com/haskell/haskell-language-server/blob/master/.github/workflows/hackage.yml)

That script checks, generates the tar.gz files, unpacks and builds them in isolation against hackage head
if the package version in the branch is different from hackage.
If the package in the branch has the same version as the released one,
it will check the relevant files have not changed and will throw an error
otherwise.

The script will upload the tarballs as candidates, maintainers will have
to check and publish them definitely.
