# Alternate Number Format Plugin

The alternate number format plugin provides alternative formatting for Numeric Literals in source code.
These can be any numeric literal such as `123`, `0x45` or any of the other numeric formats.
The Code Action will provide all possible formatting suggestions (and when required insert the associated Language Extension)

## Setup

The plugin requires no extra setup to work. Simply place your cursor on top of a literal and invoke the `codeAction` command for your editor.

## Demo

![Alternate format suggestions](HLSAll.gif)

### Currently Supported GHC Extensions:
- `BinaryLiterals`
- `HexFloatLiterals`
- `NumDecimalLiterals`

## Design

The plugin is relatively simple, it traverses a files source contents using the GHC API. As it encounters Literals (of the type `HsExpr` with the constructor of either `HsLit` or `HsOverLit`), it will construct an internal `Literal` datatype that has additional information for use to generate suggestions.
Currently, the traversal is done in the file, `Literal.hs`, using the package [SYB](https://hackage.haskell.org/package/syb) for most of the heavy lifting.

To generate suggestions, the plugin leverages the `Numeric` package which provides a multitude of conversion functions to and from strings/numerics. 

### Known Quirks
- Anything that produces a bad Source Span (i.e. can't be easily replaced by an edit) is ignored as well.

## Changelog
### 1.0.0.0
- First Release

### 1.0.1.0
- Dependency upgrades

### 1.0.1.1
- Buildable with GHC 9.2

### 1.0.2.0
- Test Suite upgraded for 9.2 semantics (GHC2021)
- Fix SYB parsing with GHC 9.2

### 1.1.0.0
- Provide ALL possible formats as suggestions
- Insert Language Extensions when needed
