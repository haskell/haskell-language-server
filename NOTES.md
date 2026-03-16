# Notes

This branch is for experimenting and improving the result of prepare-rename requests.

Currently, `prepareRenameProvider` returns `PrepareRenameDefaultBehavior True` if renaming is possible, and `PrepareRenameDefaultBehavior False` if renaming is not a valid operation.

There are two problems with this:

1. In classic TypeScript fashion, `{defaultBehavior: false}` is not specified in the LSP specification; invalid requests are supposed to result in `null` instead.

2. Not all clients support `defaultBehavior`. The most high-profile such client is the Zed editor (see [this issue](https://github.com/zed-industries/zed/issues/24184)).

## AI disclosure

The author uses generative AI (specifically, Claude Sonnet 4.6) to understand key concepts and draft code.

*The author has reviewed and understood all AI-generated content introduced in this branch, and can personally vouch for and explain it if needed.*

All notes generated through Claude are marked as such.

## Possible approach

1. Replace `PrepareRenameDefaultBehavior False` with `Null` for invalid requests.

2. Ask the client whether it supports `defaultBehavior` in the first place.

3. Return an explicit `Range` if the client does not support `defaultBehavior`, or `Null` if the cursor is somehow on an `UnhelpfulSpan`:

    1. Get the HIE AST for the current position with the `"Rename.GetHieAst"` action.

        The resulting `HieAstResult` record contains an `HieASTs a` value, which is a map. It turns out that this map contains either one AST (for the current file path) or none (if the AST is generated).

    2. Use `pointCommand` to get the smallest span inside the AST that contains the current position.

    3. Return this span as a `Range`, or return `Null` if there’s no AST.

## Testing

New test cases and a new test data file have been added. The new tests, named `prepareRenameTests` in `test/Main.hs`, perform prepare-rename requests and verify the returned responses directly using equality assertions, not through golden files.

## Other notes

1. FIXME: These lines keep appearing while running tests:

    ```
    WARNING in hptSomeThingsBelowUs
      missing module Foo
      When starting from Main
      below: {main-f16cf8d0c66360e1b6e279533ab63762122d89cc:Foo}
      Probable cause: out-of-date interface files
    ```

2. FIXME: Some tests are flaky and occasionally fail with `Exception: Language server unexpectedly terminated`.

3. IDEA: Module names can’t be renamed yet. This might be a good feature to add. (Ideally, module renaming should also rename (and relocate) the `.hs` file itself, and amend Cabal files that include it.)
