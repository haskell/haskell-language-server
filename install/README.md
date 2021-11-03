# hls-install

This directory contains the `hls-install` project, an Haskell Language Server installer.
It is used when [installing from sources](https://haskell-language-server.readthedocs.io/en/latest/installation.html#building).

Unlike other subdirectories (e.g. `hls-graph` or `ghcide`), this is not another package of the HLS project, but another project entirely (with another `stack.yaml`, another `cabal.project`, etc.). It just so happens to be in a subdirectory of HLS project for convenience, as this is tightly related to HLS.

The rationale behind this choice is to keep the installer completely isolated from main HLS code: different dependencies, different builds, etc.
