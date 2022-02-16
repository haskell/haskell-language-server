# HLint Plugin for the [Haskell Language Server](https://github.com/haskell/haskell-language-server#readme)

## Configuration

This is typically done through an [HLint configuration file](https://github.com/ndmitchell/hlint#customizing-the-hints).
You can also change the behavior of HLint by adding a list of flags to `haskell.plugin.hlint.config.flags`
if your configuration is in a non-standard location or you want to change settings globally.

## Known Differences from the `hlint` executable

- The `hlint` executable by default turns on many extensions when parsing a file because it is not certain about the exact extensions that apply to the file (they may come from project files). This differs from HLS which uses only the extensions the file needs to parse the file. Hence it is possible for the `hlint` executable to report a parse error on a file, but the `hlint` plugin to work just fine on the same file. This does mean that the turning on/off of extensions in the hlint config may be ignored by the `hlint` plugin.
- Hlint restrictions do not work (yet). This [PR](https://github.com/ndmitchell/hlint/pull/1340) should enable that functionality, but this requires a newer version of hlint to be used in HLS.

