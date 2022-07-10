# Installation

## Prerequisites

- For standalone `.hs`/`.lhs` files, [ghc](https://www.haskell.org/ghc/) must be installed and on the `PATH`. The easiest way to install it is with [ghcup](https://www.haskell.org/ghcup/) or [chocolatey](https://community.chocolatey.org/packages/ghc) on Windows.
- For Cabal based projects, both ghc and [cabal-install](https://www.haskell.org/cabal/) must be installed and on the `PATH`. It can also be installed with [ghcup](https://www.haskell.org/ghcup/) or [chocolatey](https://community.chocolatey.org/packages/cabal) on Windows.
- For Stack based projects, [stack](http://haskellstack.org) must be installed and on the `PATH`.

## ghcup

If you are using [`ghcup`](https://www.haskell.org/ghcup/) to manage your installations, you can install `haskell-language-server` with

```bash
ghcup install hls
```

You can check if HLS is available for your platform via `ghcup` here: <https://haskell.org/ghcup/install/#supported-platforms>.

You can also install HLS from source without checking out the code manually:

```bash
# `ghcup compile` uses cabal under the hood to build, so you may 
# want to run `cabal update` beforehand
cabal update
ghcup compile hls -v $HLS_VERSION --ghc $GHC_VERSION
```

Make sure to check `ghcup compile hls --help` for more complete help.

Examples:

Install HLS 1.7.0.0 for GHC 8.10.7, or for multiple GHC versions:
```bash
ghcup compile hls -v 1.7.0.0 --ghc 8.10.7
ghcup compile hls -v 1.7.0.0 --ghc 8.10.7 --ghc 9.2.3
```

Install HLS from master, or a specific commit:
```
ghcup compile hls -g master --ghc 9.2.3
ghcup compile hls -g 510bd51e46fea8fb51ddfaa60bba505f0663497d --ghc 9.2.3
```

## Installation from source

Direct installation from source, while possible via `cabal install haskell-language-server`
and `stack install --stack-yaml stack-<GHCVER>.yaml`, is not recommended for most people.
Said command builds the `haskell-language-server` binary and installs it in the default `cabal` binaries folder,
but the binary will only work with projects that use the same GHC version that built it.

### Common pre-requirements

- `stack` or `cabal` must be in your `PATH`
  - You need `stack` version >= 2.1.1 or `cabal` >= 2.4.0.0
- `git` must be in your `PATH`
- The directory where `stack`or `cabal` put the binaries must be in you PATH:
  - For `stack` you can get it with `stack path --local-bin`
  - For `cabal` it is by default `$HOME/.cabal/bin` in Linux and `%APPDATA%\cabal\bin` in windows.

Tip: you can quickly check if some command is in your path by running the command.
If you receive some meaningful output instead of "command not found"-like message
then it means you have the command in `PATH`.

### Linux-specific pre-requirements

On Linux you will need install a couple of extra libraries:

- [Unicode (ICU)](http://site.icu-project.org/)
- [NCURSES](https://www.gnu.org/software/ncurses/)
- [Zlib](https://zlib.net/)

**Debian 9/Ubuntu 18.04 or earlier**:

```bash
sudo apt install libicu-dev libtinfo-dev libgmp-dev zlib1g-dev
```

**Debian 10/Ubuntu 18.10 or later**:

```bash
sudo apt install libicu-dev libncurses-dev libgmp-dev zlib1g-dev
```

**Fedora**:

```bash
sudo dnf install libicu-devel ncurses-devel zlib-devel
```

### Windows-specific pre-requirements

In order to avoid problems with long paths on Windows you can do either one of the following:

1. Clone the `haskell-language-server` to a short path, for example the root of your logical drive (e.g. to
   `C:\hls`). Even if you choose `C:\haskell-language-server` you could hit the problem. If this doesn't work or you want to use a longer path, try the second option.

2. If the `Local Group Policy Editor` is available on your system, go to: `Local Computer Policy -> Computer Configuration -> Administrative Templates -> System -> Filesystem` set `Enable Win32 long paths` to `Enabled`. If you don't have the policy editor you can use regedit by using the following instructions [here](https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#enable-long-paths-in-windows-10-version-1607-and-later). You also need to configure git to allow longer paths by using unicode paths. To set this for all your git repositories use `git config --system core.longpaths true` (you probably need an administrative shell for this) or for just this one repository use `git config core.longpaths true`.

In addition make sure `haskell-language-server.exe` is not running by closing your editor, otherwise in case of an upgrade the executable can not be installed.

### Download the source code

```bash
git clone https://github.com/haskell/haskell-language-server
cd haskell-language-server
```

## chocolatey

If you are using [`chocolatey`](https://chocolatey.org/) to manage your installations in windows, [you can install `haskell-language-server`](https://community.chocolatey.org/packages/haskell-language-server) with

```bash
choco install haskell-language-server
```

## Visual Studio Code

If you are using Visual Studio Code, the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) will automatically download and install `haskell-language-server` for you.

If you need to find the binaries, please consult the [documentation](https://github.com/haskell/vscode-haskell#downloaded-binaries) for the extension.

## Pre-built binaries

There are pre-built binaries available from the [releases page](https://github.com/haskell/haskell-language-server/releases) for Linux, Windows and macOS.
To install, download the `haskell-language-server-wrapper` executable for your platform as well as any `haskell-language-server` executables for the GHC versions you plan on working with, and either put them on your `PATH` or point your client to them.

## Arch Linux

The preferred method of installation for development purposes is to use the [haskell-language-server-static](https://aur.archlinux.org/packages/haskell-language-server-static) package from AUR.
This package contains pre-built binaries for each supported GHC version and `haskell-language-server-wrapper` for automatic GHC version selection.
It is updated regularly, requires no additional dependencies, and is independent of other haskell packages you may have on your system, including GHC.

See [ArchWiki](https://wiki.archlinux.org/index.php/Haskell) for the details of Haskell infrastructure on Arch Linux.

## Fedora


Binary packages for Fedora are available from [this Copr repo](https://copr.fedorainfracloud.org/coprs/petersen/haskell-language-server),
built against the official Fedora ghc package.

## FreeBSD

HLS is available for installation from official binary packages. Use

```bash
pkg install hs-haskell-language-server
```

to install it. At the moment, HLS installed this way only supports the same GHC
version as the ports one.

## Gentoo

Haskell Language Server is available via the Haskell overlay. Follow the instructions [here](https://github.com/gentoo-haskell/gentoo-haskell) to install the overlay, then run:

```bash
emerge -av dev-util/haskell-language-server
```
Depending on your system setup, you may need to enable the unstable flag for this package before install, and possible also for the dependencies. If you enabled the ~testing versions as explained in the gentoo-haskell overlay instructions, then this won't be necessary.

## Installation from Hackage

Direct installation from Hackage, while possible via `cabal install haskell-language-server`, is not recommended for most people.
Said command builds the `haskell-language-server` binary and installs it in the default Cabal binaries folder,
but the binary will only work with projects that use the same GHC version that built it.

The package can be found here on Hackage: <https://hackage.haskell.org/package/haskell-language-server>

## Installation via Homebrew

Homebrew users can install `haskell-language-server` using the following command:

```bash
brew install haskell-language-server
```

This formula contains HLS binaries compiled with GHC versions available via Homebrew; at the moment those are: 8.6.5, 8.8.4, 8.10.7.

You need to provide your own GHC/Cabal/Stack as required by your project, possibly via Homebrew.

## Installation using nix

You can read full instructions on how to install HLS with nix in [haskell4nix documentation](https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-install-haskell-language-server).
