# Installation

## Prerequisites

- For standalone `.hs`/`.lhs` files, [ghc](https://www.haskell.org/ghc/) must be installed and on the PATH. The easiest way to install it is with [ghcup](https://www.haskell.org/ghcup/) or [chocolatey](https://community.chocolatey.org/packages/ghc) on Windows.
- For Cabal based projects, both ghc and [cabal-install](https://www.haskell.org/cabal/) must be installed and on the PATH. It can also be installed with [ghcup](https://www.haskell.org/ghcup/) or [chocolatey](https://community.chocolatey.org/packages/cabal) on Windows.
- For Stack based projects, [stack](http://haskellstack.org) must be installed and on the PATH.

## ghcup

If you are using [`ghcup`](https://www.haskell.org/ghcup/) to manage your installations, you can install `haskell-language-server` with
```bash
ghcup install hls
```

You can check if HLS is available for your platorm via ghcup here: <https://gitlab.haskell.org/haskell/ghcup-hs#supported-platforms>.

You can also install HLS from source without checking out the code manually:
```bash
ghcup compile hls -v 1.4.0 8.10.7
```

Check `ghcup compile hls --help` for a full list of compilation options.

## chocolatey

If you are using [`chocolatey`](https://chocolatey.org/) to manage your installations in windows, [you can install `haskell-language-server`](https://community.chocolatey.org/packages/haskell-language-server) with
```bash
choco install haskell-language-server
````

## Visual Studio Code

If you are using Visual Studio Code, the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) will automatically download and install `haskell-language-server` for you.

## Pre-built binaries

There are pre-built binaries available from the [releases page](https://github.com/haskell/haskell-language-server/releases) for Linux, Windows and macOS.
To install, download the `haskell-language-server-wrapper` executable for your platform as well as any `haskell-language-server` executables for the GHC versions you plan on working with, and either put them on your PATH or point your client to them.

## Arch Linux

The preferred method of installation for development purposes is to use the [haskell-language-server-bin](https://aur.archlinux.org/packages/haskell-language-server-bin) package from AUR.
This package contains statically linked binaries for each supported GHC version and `haskell-language-server-wrapper` for automatic GHC version selection.
It is updated regularly, requires no additional dependencies, and is independent of other haskell packages you may have on your system, including GHC.
Its size is relatively large (approx. 900 MB), but if this is a problem for you, during installation you can disable the GHC versions you will not be using by editing the PKGBUILD file.

Alternatively, if you want to use **dynamically linked** Haskell packages from `pacman`,
you can install the latest pre-compiled version of `haskell-language-server` from [[community]](https://archlinux.org/packages/community/x86_64/haskell-language-server/):

```bash
sudo pacman -S haskell-language-server
```

In this case, `haskell-language-server` is compiled against the GHC distributed to Arch Linux, so you will need maintain a system wide Haskell development environment, and install GHC from `pacman` as well.
See [ArchWiki](https://wiki.archlinux.org/index.php/Haskell) for the details of Haskell infrastructure on Arch Linux.

## FreeBSD

HLS is available for installation from official binary packages. Use

```bash
pkg install hs-haskell-language-server
```

to install it. At the moment, HLS installed this way only supports the same GHC
version as the ports one.



## Installation from source

### Common pre-requirements

- `stack` or `cabal` must be in your PATH
  - You need stack version >= 2.1.1 or cabal >= 2.4.0.0
- `git` must be in your PATH
- The directory where `stack`or `cabal` put the binaries must be in you PATH:
  - For stack you can get it with `stack path --local-bin`
  - For cabal it is by default `$HOME/.cabal/bin` in linux and `%APPDATA%\cabal\bin` in windows.

Tip: you can quickly check if some command is in your path by running the command.
If you receive some meaningful output instead of "command not found"-like message
then it means you have the command in PATH.

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
git clone https://github.com/haskell/haskell-language-server --recurse-submodules
cd haskell-language-server
```

### Building

Note, on first invocation of the build script with stack, a GHC is being installed for execution.
The GHC used for the `install.hs` can be adjusted in `./install/stack.yaml` by using a different resolver.

Available commands can be seen with:

```bash
stack ./install.hs help
```

Remember, this will take time to download a Stackage-LTS and an appropriate GHC for build
haskell-language-server the first time.

### Install via cabal

The install-script can be invoked via `cabal` instead of `stack` with the command

```bash
cabal v2-run ./install.hs --project-file install/shake.project <target>
```

or using the existing alias script

```bash
./cabal-hls-install <target>
```

Running the script with cabal on windows requires a cabal version greater or equal to `3.0.0.0`.

For brevity, only the `stack`-based commands are presented in the following sections.

### Install specific GHC Version

The script will install the executables `haskell-language-server-wrapper` and `haskell-language-server`.

It will copy the latter appending the used ghc version, needed by the wrapper to choose the suitable version
for the project at hand.

So installing the executables directly with `stack install` or `cabal v2-install` may not be enough
for it to work properly.

Install haskell-language-server for the latest available and supported GHC version (and hoogle docs):

```bash
stack ./install.hs hls
```

Install haskell-language-server for a specific GHC version (and hoogle docs):

```bash
stack ./install.hs hls-8.8.3
```

`hls-8.8.3` target will build the project and install `haskell-language-server-wrapper`,
`haskell-language-server`, `haskell-language-server-8.8.3` and `haskell-language-server-8.8`
executables.

The Haskell Language Server can also be built with `cabal v2-build` instead of `stack build`.
This has the advantage that you can decide how the GHC versions have been installed.
To see what GHC versions are available, the command `cabal-hls-install ghcs` can be used.
It will list all *supported* GHC versions that are on the path for build with their respective installation directory.
If you think, this list is incomplete, you can try to modify the PATH variable, such that the executables can be found.
Note, that the targets `hls` and `data` depend on the found GHC versions.

An example output is:

```bash
> ./cabal-hls-install ghcs
******************************************************************
Found the following GHC paths:
ghc-8.6.5: /opt/bin/ghc-8.6.5
ghc-8.8.3: /opt/bin/ghc-8.8.3

******************************************************************
```

If your desired ghc has been found, you use it to install haskell-language-server.

```bash
./cabal-hls-install hls-8.6.5
```

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

