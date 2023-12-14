# Semantic tokens (LSP) plugin for Haskell language server

## Purpose

The purpose of this plugin is to provide semantic tokens for the Haskell language server,
according to the [LSP specification](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens)
It can be used to provide semantic highlighting for Haskell code in editors by given semantic type and modifiers for some tokens.
A lot of editors support semantic highlighting through LSP, for example vscode, vim, emacs, etc.

## Features

### Semantic types and modifiers

The handles request for semantic tokens for the whole file.
It supports semantic types and but not yet modifiers from the LSP specification.

Default semantic types defined in lsp diverge greatly from the ones used in ghc.
But default semantic types allows user with less configuration to get semantic highlighting.
That is why we use default semantic types for now. By mapping ghc semantic types to lsp semantic types.
The mapping is defined in `Mapping.hs` file.

### Delta semantic tokens and range semantic tokens

1. It is not yet support capabilities for delta semantic tokens, which might be
crucial for performance.

2. range semantic tokens are not yet supported.

