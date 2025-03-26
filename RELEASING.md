# Releasing

## Release checklist

- [ ] check ghcup supports new GHC releases if any
- [ ] check all plugins still work if release includes code changes
- [ ] set the supported GHCs in workflow file `.github/generate-ci/gen_ci.hs`
- [ ] regenerate the CI via `./.github/generate-ci/generate-jobs`
- [ ] bump package versions in all `*.cabal` files (same version as hls)
  - HLS uses lockstep versioning. The core packages and all plugins use the same version number, and only support exactly this version.
    - Exceptions:
      - `hie-compat` requires no automatic version bump.
      - `shake-bench` is an internal testing tool, not exposed to the outside world. Thus, no version bump required for releases.
  - For updating cabal files, the following script can be used:
    - ```sh
      ./release/update_versions.sh <OLD_VERSION> <NEW_VERSION>
      ```
    - It still requires manual verification and review
- [ ] generate and update changelog
  - Generate a ChangeLog via `./GenChangelogs.hs <api-key> <tag>`
    - `<tag>` is the git tag you want to generate the ChangeLog from.
    - `<api-key>` is a github access key: https://github.com/settings/tokens
- [ ] update https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html#current-ghc-version-support-status
- [ ] create release branch as `wip/<version>`
  - `git switch -c wip/<version>`
- [ ] create release tag as `<version>`
  - `git tag <version>`
- [ ] trigger release pipeline by pushing the tag
  - this creates a draft release
  - `git push <remote> <version>`
- [ ] run `sh scripts/release/download-gh-artifacts.sh <version> <your-gpg-email>`
  - downloads artifacts to `gh-release-artifacts/haskell-language-server-<version>/`
  - also downloads FreeBSD bindist from circle CI
  - adds signatures
- [ ] upload artifacts to downloads.haskell.org from `gh-release-artifacts/haskell-language-server-<version>/`
  - You require sftp access, contact wz1000, bgamari or chreekat
  - `cd gh-release-artifacts/haskell-language-server-<version>`
  - `SIGNING_KEY=... ../../release/upload.sh upload`
    - Your SIGNING_KEY can be obtained with `gpg --list-secret-keys --keyid-format=long`
  - Afterwards, the artifacts are available at: `https://downloads.haskell.org/~hls/haskell-language-server-<version>/`
  - Run `SIGNING_KEY=... ../../release/upload.sh purge_all` to remove CDN caches
- [ ] create PR to [ghcup-metadata](https://github.com/haskell/ghcup-metadata)
  - [ ] update `ghcup-vanilla-0.0.8.yaml` and `ghcup-vanilla-0.0.7.yaml`
    - can use `sh scripts/release/create-yaml-snippet.sh <version>` to generate a snippet that can be manually inserted into the yaml files
  - ~~update `hls-metadata-0.0.1.json`~~ Currently unnecessary, GHCup builds its own HLS binaries and updates that file.
    - utilize `cabal run ghcup-gen -- generate-hls-ghcs -f ghcup-0.0.7.yaml --format json --stdout` in the root of ghcup-metadata repository
  - Be sure to mark the correct latest version and add the 'recommended' tag to the latest release.
- [ ] get sign-off on release
  - from wz1000, michealpj, maerwald and fendor
- [ ] publish release on github
- [ ] upload hackage packages
  - requires credentials
- [ ] Supported tools table needs to be updated:
  - https://www.haskell.org/ghcup/install/#supported-platforms
  - https://github.com/haskell/ghcup-hs/blob/master/docs/install.md#supported-platforms
  - https://github.com/haskell/ghcup-metadata/blob/44c6e2b5d0fcae15abeffff03e87544edf76dd7a/ghcup-gen/Main.hs#L67
- [ ] post release on discourse and reddit
- [ ] merge release PR to master or forward port relevant changes
