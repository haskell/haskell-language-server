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

* Supported semantic tokens:
  * [x] class and class method
  * [x] type family name
  * [x] data constructor name (not distinguishing record and normal data)
  * [x] type constructor name
  * [x] record field name
  * [x] type synonym
  * [x] pattern synonym
  * [x] pattern bindings (map to parameters in standard token)
  * [x] value bindings (map to functions in standard token)
  * [x] imported name

* Supported modifiers:
  * [ ] declaration (as in class declarations and type family)
  * [ ] definition (as in class instance declaration and type family instance)
  * [ ] modification (as in rec field update)

## Semantic highlighting sample

![semantic highlighting sample](https://private-user-images.githubusercontent.com/14073857/290981908-9619fae2-cb92-4d4e-b8f8-6507851ba9f3.png?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTEiLCJleHAiOjE3MDMyOTAzMjcsIm5iZiI6MTcwMzI5MDAyNywicGF0aCI6Ii8xNDA3Mzg1Ny8yOTA5ODE5MDgtOTYxOWZhZTItY2I5Mi00ZDRlLWI4ZjgtNjUwNzg1MWJhOWYzLnBuZz9YLUFtei1BbGdvcml0aG09QVdTNC1ITUFDLVNIQTI1NiZYLUFtei1DcmVkZW50aWFsPUFLSUFJV05KWUFYNENTVkVINTNBJTJGMjAyMzEyMjMlMkZ1cy1lYXN0LTElMkZzMyUyRmF3czRfcmVxdWVzdCZYLUFtei1EYXRlPTIwMjMxMjIzVDAwMDcwN1omWC1BbXotRXhwaXJlcz0zMDAmWC1BbXotU2lnbmF0dXJlPTNiODU1MTBhNTRhNTFhNmYzZjRjYTM1MzViZjNhMDgwYTZiODk3OTA4YzFmMTQ1ZjhhNDcyYWMyMjFlMDQwMDYmWC1BbXotU2lnbmVkSGVhZGVycz1ob3N0JmFjdG9yX2lkPTAma2V5X2lkPTAmcmVwb19pZD0wIn0._lgQ0lAGUD5nUkRx_s2rWxTS81ezqKr71ad2fWHGDfk)
