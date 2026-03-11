# Notes

This branch is for experimenting and attempting to implement renaming qualified-as aliases. For example:

```haskell

-- Before: ----------------------------

import qualified Data.List as L

bar = L.take

-- After: -----------------------------

import qualified Data.List as List

bar = List.take
```

## AI disclosure

The author uses generative AI (specifically, Claude Sonnet 4.6) to understand key concepts and draft code.

*The author has reviewed and understood all AI-generated content introduced in this branch, and can personally vouch for and explain it if needed.*

All notes generated through Claude are marked as such.

## How renaming currently works

When the user hovers the cursor over `L` or `L.take`, the `hls-rename-plugin` consults GHC’ AST and determines the identifier at point. An identifier is of type `Identifier`, defined as `type Identifier = Either ModuleName Name`.

- In `import qualified Data.List as L`, `L` is considered a `ModuleName`.
- In `L.take`, the entirety of `L.take` is considered a single `Name`. The AST records the name as external, coming from the `Data.List` module (not the `L` module, because the name is resolved).

The plugin only considers `Name` identifiers as eligible for renaming. Therefore, if the user hovers over `L` in `import qualified Data.List as L`, the plugin says “No symbol to rename at given location.”

Also, a `Name` records the module in which the identifier is *defined*, not the module that the identifier is imported from. This can cause problems, especially for modules that re-export other modules.

## Possible approach

1. When the user hovers over `L.take`, use the HIE AST to get the full span of the identifier.
2. Use [`GHC.Parser`](https://hackage-content.haskell.org/package/ghc-9.12.1/docs/GHC-Parser.html) to parse the identifier into a `RdrName` of the form `Qual ModuleName OccName` and obtain the module alias as listed in the import section.
3. Use the AST to find all other external identifiers (and check using the `isExternalName` predicate).
4. Find their spans and parse these identifiers into `RdrName` values to find those with the same qualified module alias.
5. Replace the module alias in both the import statement and the identifiers.

## Revised approach

> From the session summary:
>
> 1. Get the parsed AST (`HsModule GhcPs`) via `GetParsedModule`.
> 2. Traverse `hsmodImports` to find the `ImportDecl` whose `ideclAs` span contains the cursor position.
> 3. Traverse all `HsVar` nodes in `hsmodDecls`, collect those with `Qual alias _` RdrNames matching the target alias — extract their `SrcSpan`s directly from the AST.
> 4. Replace the alias text in the `ideclAs` span in the import, and each collected use-site span.
> 5. Produce a `WorkspaceEdit` with all replacements.
>
> —Claude Sonnet 4.6

Using the parsed AST instead of the full HIE AST allows us to inspect `RdrName` identifiers, which contain unresolved import module aliases. It also turns out that this is already implemented as a rule in HLS.
