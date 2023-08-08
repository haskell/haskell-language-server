# Releasing

## Release checklist

- [ ] check ghcup supports new GHC releases if any
- [ ] set the supported GHCs in workflow file `.github/workflows/release.yaml`
- [ ] check all plugins still work if release includes code changes
- [ ] bump package versions in all `*.cabal` files (same version as hls)
  - HLS uses a lockstep versioning. The core packages and all plugins use the same version number, and only support exactly the this version.
    - Exceptions:
      - `hie-compat` requires no automatic version bump.
      - `shake-bench` is an internal testing tool, not exposed to the outside world. Thus, no version bump required for releases.
  - For updating cabal files, the following script can be used:
    - ```sh
      # Update all `version:` fields
      sed -ri "s/^version:( +)2.1.0.0/version:\12.2.0.0/" **/*.cabal
      # Update all constraints expected to be in the form `== <version>`.
      # We usually don't force an exact version, so this is relatively unambiguous.
      # We could introduce some more ad-hoc parsing, if there is still ambiguity.
      sed -ri "s/== 2.1.0.0/== 2.2.0.0/" **/*.cabal
      ```
    - It still requires manual verification and review
- [ ] generate and update changelog
  - Generate a ChangeLog via `./GenChangelogs.hs <api-key> <tag>`
    - `<tag>` is the git tag you want to generate the ChangeLog from.
    - `<api-key>` is a github access key: https://github.com/settings/tokens
- [ ] create release branch as `wip/<version>`
  - `git switch -c wip/<version>`
- [ ] create release tag as `<version>`
  - `git tag <version>`
- [ ] trigger release pipeline by pushing the tag
  - this creates a draft release
  - `git push <remote> <version>`
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
