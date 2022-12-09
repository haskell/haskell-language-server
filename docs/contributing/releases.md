# Releases and distributable binaries

Starting with 0.2.1.0 haskell-language-server provides pre-built binaries on
each [GitHub release](https://github.com/haskell/haskell-language-server/releases).
These binaries are used by the [vscode-haskell extension](https://github.com/haskell/vscode-haskell)
to provide automatic installation for users on VS Code, but they can also be installed manually
when added to the path.

Starting with 0.8.0.0 haskell-language-server and all its related packages
(core libraries like ghcide, plugins and hls itself) is being released on
[hackage](https://hackage.haskell.org/package/haskell-language-server) as well.
This allows cabal users to install it with `cabal install haskell-language-server`
and it is being used in nix environments.

Since 1.7.0.0, HLS binaries are no longer uploaded to GitHub but to [downloads.haskell.org](https://downloads.haskell.org/~hls/).
[GHCup](https://www.haskell.org/ghcup/) uses these binaries to enable automatic installation of HLS binaries in
various lsp-client plugins, such as [vscode-haskell](https://github.com/haskell/vscode-haskell).

## Minimal checklist

### prerelease sanity checks

- [ ] check that all plugins work according to their [support tiers](../support/plugin-support.md)
- [ ] set the supported GHC versions and their corresponding cabal project-files in `bindist/ghcs` according to the [GHC version deprecation policy](../support/ghc-version-support.md#ghc-version-deprecation-policy)
- [ ] [trigger manually](https://docs.github.com/es/actions/managing-workflow-runs/manually-running-a-workflow) the hackage workflow *without* uploading the packages
- [ ] trigger manually the build workflow
- [ ] create a prerelease tag `${version}-check-gitlab` and push it to the [project repo in gitlab](https://gitlab.haskell.org/haskell/haskell-language-server) to check the build is fine

### github release

- [ ] generate the list of pull requests finished since the last release using the [haskell script](https://github.com/haskell/haskell-language-server/blob/master/GenChangelogs.hs) in the project root.
  Nix users should run command `gen-hls-changelogs` (a wrapper of the script) in nix-shell instead.
- [ ] add that list to the actual [Changelog](https://github.com/haskell/haskell-language-server/blob/master/ChangeLog.md) with a description of the release.
- [ ] bump up versions of changed packages. All are optional but [haskell-language-server itself](https://github.com/haskell/haskell-language-server/blob/master/haskell-language-server.cabal).
- [ ] create the tag and make an initial prerelease to trigger the ci workflow (see details below)
- [ ] contact ghcup team (#haskell-ghcup irc channel or via its [repo](https://github.com/haskell/ghcup-metadata)) to try to sync our release and its inclusion in ghcup
- [ ] in the github release edit page, check the attached binaries and the release description (usually the changelog entry) and uncheck the prerelease box
- [ ] make public the release in the usual social channels (not required but useful to spread the word :slightly_smiling_face:):
  - [ ] irc
  - [ ] matrix
  - [ ] twitter
  - [ ] discord
  - [ ] discourse
  - [ ] reddit

### hackage release

- [ ] bump up package versions following the [pvp specification](https://pvp.haskell.org/) if they are not already updated. You could use [policeman](https://github.com/kowainik/policeman) to help with this step.
- [ ] create ${version}-hackage branch to trigger the hackage github workflow which will upload all changed packages to hackage as candidates
- [ ] for new plugins or packages, update hackage uploaders to add the author of the plugin/package and some hls maintainer(s) other than the owner of the hackage api key used to upload them (it has to be done by the owner of the api key, actually @pepeiborra)
- [ ] check manually candidates in hackage
- [ ] publish them definitely triggering a manual run of the hackage workflow setting the upload and publish inputs to `true`

### ghcup release

- [ ] push the release tag to the [haskell-language-server gitlab repo](https://gitlab.haskell.org/haskell/haskell-language-server) to trigger the build of ghcup specific artifacts
- [ ] download specific artifacts [only available in the gitlab build](#haskell-gitlab-release-pipeline) and compute their sha256sum
- [ ] upload them to the github release and complete the SHA256SUMS file
- [ ] change ghcup metadata to include the new release in <https://github.com/haskell/ghcup-metadata>
  - example pull request [here](https://github.com/haskell/ghcup-metadata/pull/11)

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

### Failing workflow

If the workflow fail and some binaries has been already uploaded,
those artifacts must be removed and the build should be re-ran (the build tries to upload themm all and it fails if there is an existing artifact with the same name)

### Updating release artifacts

*IMPORTANT: release artifacts must not be modified, cause it would break
its secure distribution using their hashes. We should only add new ones.*

To manually upload a new binary we should:

- Add the new tar/zip following the name conventions of existing ones
  - `haskell-language-server-${os}-${ghcVersion}.gz` for `Linux` and `macOS` and `haskell-language-server-Windows-${ghcVersion}.exe.zip` for `Windows`
  - the binary inside the gz file is named `haskell-language-server-${ghcVersion}` (with the `.exe` extension for `Windows`). Note that the binary name does not contain the `${os}` part.
- Add the executable to the existing tar `haskell-language-server-${os}-${ghcVersion}.tar.gz` *locally* and upload it under a new name `haskell-language-server-${os}-${ghcVersion}-rev${n}.tar.gz` following the same schema for the binary as the previous one.
  - `-rev${n}` is the next revision number of the tarball, starting at 1.
  - we should contact users of the tarball (particularly ghcup) to notify the change

## Hackage release workflow

We aim to do hackage releases following the github ones described above.
To help in that job we have added a [github workflow](https://github.com/haskell/haskell-language-server/blob/master/.github/workflows/hackage.yml)

That script checks, generates the tar.gz files, unpacks and builds them in isolation
against hackage head if the package version in the branch is different from hackage.
If the package in the branch has the same version as the released one, it will check
the relevant files have not changed and will throw an error otherwise.

You can trigger the build manually.

The script will upload the tarballs as candidates, maintainers will have to check and publish them definitely.

## haskell gitlab release pipeline

The project is present in the haskell gitlab server: <https://gitlab.haskell.org/haskell/haskell-language-server>
The main motivation is to leverage the ci infrastructure which includes architectures not included in the github ci.
The specific architectures only available through gitlab are: `aarch64-darwin`, `aarch64-linux`, `armv7-linux`, `x86_64-freebsd12`, `x86_64-freebsd13`, `x86_64-linux-alpine`
The gitlab pipeline uses the configuration file [.gitlab-ci.yml](https://github.com/haskell/haskell-language-server/blob/master/.gitlab-ci.yml)
and the sh scripts in [.gitlab](https://github.com/haskell/haskell-language-server/tree/master/.gitlab)
It is triggered by pushing a tag to the gitlab repo.
