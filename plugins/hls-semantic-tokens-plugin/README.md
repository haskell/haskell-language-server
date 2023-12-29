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

### delta semantic tokens, range semantic tokens and refresh

It is not yet support capabilities for delta semantic tokens, which might be
crucial for performance.
It should be implemented in the future.

## checkList

* Supported PluginMethodHandler
  * [x] [textDocument/semanticTokens/full](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokens_fullRequest).
  * [ ] [textDocument/semanticTokens/full/delta](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokens_deltaRequest)
  * [ ] [workspace/semanticTokens/refresh](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokens_refreshRequest)

* Supported semantic tokens type:
  * [x] class and class method
  * [x] type family name (data family)
  * [x] data constructor name (not distinguishing record and normal data, and GADT)
  * [x] type constructor name (GADT)
  * [x] record field name
  * [x] type synonym
  * [x] pattern synonym
  * [x] ~~pattern bindings~~ In favor of differing functions and none-functions from its type
  * [x] ~~value bindings~~ In favor of differing functions and none-functions from its type
  * [x] functions
  * [x] none-function variables
  * [x] imported name

* Supported modifiers(planning):
  * [future] declaration (as in class declearations, type definition and type family)
  * [future] definition (as in class instance declaration, left hand side value binding, and type family instance)
  * [future] modification (as in rec field update)

## Implementation details

* [x] Compute visible names from renamedsource
* [x] Compute `NameSemanticMap` for imported and top level name tokens using `HscEnv`(with deps) and type checked result
* [x] Compute current module `NameSemanticMap` using `RefMap a` from the result of `GetHieAst`
* [x] Compute all visible `(Name, Span)` in current module, in turn compute their semantic token using the combination map of the above two `NameSemanticMap`
* [x] use default legends, Candidates map of token type with default token type: [Maps to default token types](https://github.com/soulomoon/haskell-language-server/blob/master/plugins/hls-semantic-tokens-plugin/src/Ide/Plugin/SemanticTokens/Mappings.hs)
* [x] add args support to turn the plugin on and off
* [x] enhence test <https://github.com/haskell/haskell-language-server/pull/3892#discussion_r1427844520>
* [x] enhence error reporting. <https://github.com/haskell/haskell-language-server/pull/3892#discussion_r1427955335>
* [x] computation of semantic tokens is pushed into rule `getSemanticTokensRule`
* [future] make use of modifiers
* [future] hadling customize legends using server capabilities (how?)
