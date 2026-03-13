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
3. Return an explicit `Range` if the client does not support `defaultBehavior`, or `Null` if the cursor is somehow on an `UnhelpfulSpan`.
