# Notes

This branch is for experimenting and attempting to implement renaming import aliases. For example:

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

*The author has reviewed and understood every line of AI-generated content in this branch, personally vouches for it, and can explain any part of it if needed.*

All notes generated through Claude are marked as such.

## How renaming currently works

`hls-rename-plugin` currently doesn’t handle import aliases.

Consider the `-- Before: --` example above. When the user places the cursor on either occurrence of `L`, `hls-rename-plugin` consults the HIE AST and finds the identifier at the cursor.

- In `import qualified Data.List as L`, `L` is a `ModuleName` identifier.

    The plugin currently doesn’t consider `ModuleName` identifiers renameable, so it responds with “No symbol to rename at given location.”

- In `L.take`, the entirety of `L.take` is a single `Name` identifier.

    The AST records the name as external, coming from the `Data.List` module (not `L`, because the import alias is resolved). Information about the alias `L` is therefore lost.

    Also, a `Name` records the module in which the identifier is *defined*, not the module that the identifier is imported from. This can cause problems, especially for modules that re-export other modules.

## Initial approach

1. When the user hovers over `L.take`, use the HIE AST to get the full span of the identifier.
2. Use [`GHC.Parser`](https://hackage-content.haskell.org/package/ghc-9.12.1/docs/GHC-Parser.html) to parse the identifier into a `RdrName` of the form `Qual ModuleName OccName` and obtain the module alias as listed in the import section.
3. Use the AST to find all other external identifiers (and check using the `isExternalName` predicate).
4. Find their spans and parse these identifiers into `RdrName` values to find those with the same qualified module alias.
5. Replace the module alias in both the import statement and the identifiers.

## Revised approach

> 1. Get the parsed AST (`HsModule GhcPs`) via `GetParsedModule`.
> 2. Determine the alias being renamed by checking two cursor positions, in order:
>    - **2a.** The cursor is on the alias token in an import declaration — traverse `hsmodImports` to find the `ImportDecl` whose `ideclAs` span contains the cursor.
>    - **2b.** The cursor is on a qualifier at a use site — traverse `hsmodDecls` to find a `Qual moduleAlias _` `RdrName` whose qualifier span contains the cursor, then look up the matching `ideclAs` in `hsmodImports`.
>
>    If multiple imports share the same alias, fall back to the renamed AST via a fresh `TypeCheck` to disambiguate.
>
>    Both yield `(ModuleName, RealSrcSpan)`: the alias name and its span in the import declaration.
> 3. Traverse all `LocatedN RdrName` nodes in `hsmodDecls` via SYB `listify`, collect those with `Qual alias _` matching the target alias — extract their `RealSrcSpan`s from the annotation.
> 4. Replace the alias text in the `ideclAs` span in the import, and each collected use-site span.
> 5. Produce a `WorkspaceEdit` with all replacements.
>
> —Claude

1. Using the parsed AST instead of the full HIE AST allows us to inspect `RdrName` identifiers, which contain unresolved import module aliases. It also turns out that this is already implemented as a rule in HLS.

2. The cursor can be on either an import alias declaration (such as `L` in `import Data.List as L`) or a use site (such as `L` in `L.take`).

    - Common case (fast): If only one module is imported as `L`, then all renaming is done through the parsed AST, which is fast.

    - Special case (slow, but rare): If multiple modules are imported as `L`, then `hls-rename-plugin` consults the typechecked AST and `GlobalRdrEnv` to find which imported module is referred to.

        For example, targeting `L` in `L.take` in this example leaves the `Control.Lens` import unchanged:

        ```haskell
        import Control.Lens as L  -- >  import Control.Lens as L
        import Data.List as L     -- >  import Data.List as List
                                  -- >
        f = L.take                -- >  f = List.take
        ```

        Relevant link: [GHC wiki page on the renamer](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/renamer?version_id=9dccaa3e023565a2ef5091b4a08da847872714ff)

        - NOTE: If any import declarations mention a module not found in any dependencies (like `Control.Lens` without `lens`), then typechecking fails, and the plugin can’t disambiguate the alias. This case should result in a meaningful error message.

            This also means disambiguation can only happen after typechecking is complete. (Also, typechecking and renaming can be thought of as happening in the same pass.[^1] [^2])

        - ~~TODO~~ DONE: Handle when both a module and its re-exporter are imported, like this:

            ```haskell
            import Control.Lens as L
            import Control.Lens.Getter as L

            f = L.view
            ```

            - If the cursor is on `L` in `L.view`, then throw an error like “Can’t rename: There are 2 matching imports with the alias 'L'. Click on one of these 'L' aliases in the import declarations and try renaming again.”
            - If the cursor is on `L` in either `as L` declaration, then rename all entities exported by the corresponding module (including re-exported ones).

3. Traversing the AST is done using `listify` from `syb`. `listify` needs to be monomorphic. To apply the correct type, use the `XRec` and `Anno` type families.

4. GHC source spans are in Unicode code points; LSP uses UTF-16 code units by default.

    **Avoid any direct arithmetic between `RealSrcSpan` and `Range`.**

    Use `Language.LSP.VFS.CodePointRange` instead.[^3]

5. There are three key design questions, generated by Claude:

    > Here's step 5 — assembling the `WorkspaceEdit` and wiring everything together. A few design questions to settle before I write the code:
    >
    > 1. **Entry point**: should alias renaming be a separate branch inside the existing `renameProvider`/`prepareRenameProvider` handlers, or a completely separate handler registered alongside them?
    >
    > 2. **`prepareRenameProvider`**: for an alias rename, what should the prepare response return? The default range (the alias token at cursor) and the current alias text seems right, but should it return a `PrepareRenameResult` with a `defaultBehavior` or an explicit `range` + `placeholder`?
    >
    > 3. **Error handling**: if `getParsedModuleStale` returns `Nothing` (e.g. file hasn't been parsed yet), should we fall through to the existing name-based rename, or fail with an explicit error message?
    >
    > —Claude

    The following design decisions are taken by the author:

    1. Add a branch inside the existing `Provider` handlers, because registering multiple `renameProvider` handlers would be difficult and impractical. The new alias-renaming branch takes place first in both handlers, ahead of the existing renaming logic.

    2. `prepareRenameProvider` returns `PrepareRenameDefaultBehavior True` if the cursor is on a renameable alias.

        TODO: In general, `PrepareRenameResult` feels underutilized.

    3. Fail early if HLS can’t get the parsed module, instead of falling through. The existing renaming logic also needs the module to be parsed (and then typechecked) anyway.

[^1]: https://ghc-proposals.readthedocs.io/en/latest/proposals/0107-source-plugins.html

[^2]: https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html

[^3]: https://github.com/haskell/haskell-language-server/issues/2646#issuecomment-1024990401
