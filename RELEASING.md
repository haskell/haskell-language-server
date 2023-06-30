# Releasing

## Release checklist

- [ ] check ghcup supports new GHC releases if any
- [ ] set the supported GHCs in workflow file `.github/workflows/release.yaml`
- [ ] check all plugins still work if release includes code changes
- [ ] bump package versions in all `*.cabal` files (same version as hls)
- [ ] generate and update changelog
- [ ] create release branch as `wip/<version>`
- [ ] create release tag as `<version>`
- [ ] trigger release pipeline by pushing the tag
  - this creates a draft release
- [ ] run `sh scripts/release/download-gh-artifacts <version> <your-gpg-email>`
  - downloads artifacts to `gh-release-artifacts/<version>/`
  - also downloads FreeBSD bindist from circle CI
  - adds signatures
- [ ] upload artifacts to downloads.haskell.org manually from `gh-release-artifacts/<version>/`
- [ ] create PR to [ghcup-metadata](https://github.com/haskell/ghcup-metadata)
  - [ ] update `ghcup-0.0.7.yaml` and `ghcup-vanilla-0.0.7.yaml`
    - can use `sh scripts/release/create-yaml-snippet.sh <version>` to generate a snippet that can be manually inserted into the yaml files
  - [ ] update `hls-metadata-0.0.1.json`
    - utilize `cabal run ghcup-gen -- generate-hls-ghcs -f ghcup-0.0.7.yaml --format json --stdout` in the root of ghcup-metadata repository
- [ ] get sign-off on release
  - from wz1000, michealpj, maerwald and fendor
- [ ] publish release on github
- [ ] upload hackage packages
  - requires credentials
- [ ] update https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html#current-ghc-version-support-status
- [ ] post release on discourse and reddit
- [ ] merge release PR to master or forward port relevant changes
