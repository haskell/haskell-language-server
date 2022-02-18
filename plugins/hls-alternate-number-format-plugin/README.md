# Alternate Number Format Plugin

The alternate number format plugin provides alternative formatting for Numeric Literals in source code.
These can be any numeric literal such as `123`, `0x45` or any of the other numeric formats.
The plugin is context aware and will provide suggestions based on currently active GHC extensions.

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

The plugin extends on top of SYB as the traversal done by basic combinators is not perfect. For whatever reason, when starting at the root `ParsedModule` the SYB traversal ignores Pattern Binds (`LPat GhcPs`). As a result, a combinator was created to match on TWO separate underlying types to dispatch on.

To generate suggestions, the plugin leverages the `Numeric` package which provides a multitude of conversion functions to and from strings/numerics. The only slight change is the addition of extra work when using `NumDecimals` extension. The plugin will attempt to generate 3 choices for the user (this choice is not given for `Fractional` numerics).

### Known Quirks
- Currently (and probably inefficiently), a Set is used as general accumulator for all Literals being captured. This is because again, through the intricacies of using SYB, we somehow will traverse Source Text multiple times and collect duplicate literals.

- In the Test Suite, we are required to be explicit in where our `codeActions` will occur. Otherwise, a simple call to `getAllCodeActions` will not work, for whatever reason, there is not enough time to generate the code actions.

- `PrimLiterals` are currently ignored. GHC API does not attach Source Text to Primitive Literal Nodes. As such these are ignored in the plugin.

- Similarly, anything that produces a bad Source Span (i.e. can't be easily replaced by an edit) is ignored as well.

## Changelog
### 1.0.0.0
- First Release

### 1.0.1.0
- Dependency upgrades

### 1.0.1.1
- Buildable with GHC 9.2
