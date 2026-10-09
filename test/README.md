# The `func-test` test suite.

This is the integration test suite for cross-plugin and cross-package features.

Add integration tests to `func-test` only if they satisfy one or more of the following conditions:

* It tests the interaction between more than one plugin.
  * For example, plugin A provides a Diagnostic that plugin B requires to provide a CodeAction.
  * However, it is also valid, and often preferable, to depend on the required plugin directly in plugin B's test suite.
* It tests HLS specific LSP code.
  * For example, we test that config changes are appropriately propagated.
    * Note, this is slightly debatable, since the test could also be part of `ghcide`.
  * Non HLS specific LSP code may exist in HLS temporarily, but any LSP extensions should be upstreamed to `lsp`.
* It tests features of the `haskell-language-server-wrapper` executable.
  * For example, argument parsing.
* It tests features of the `haskell-language-server` executable.
  * For example, argument parsing.
* It tests features provided by `hls-plugin-api` that require an integration test (i.e. a unit test doesn't suffice).
  * Example: Testing the Logger setup.

If you think that a test that currently lives in `func-test` does not meet the conditions above, open a ticket for discussion or try to move the test to a better location.

Note: `func-test` is a historical test suite. It was originally written for Haskell IDE Engine, which was merged with the `ghcide` project.
The integration test-suite `func-test` (back then `unit-test` existed as well) was used to test all kinds of features provided by Haskell IDE Engine (HIE).
When `ghcide` and HIE merged together, the integration test suite was vastly copied.
HLS moved to a plugin-based architecture, which mainly entails that plugin tests are isolated in the respective plugin's test suite.
Over time, `func-test` started to bit rot and wasn't maintained properly any more, since all new tests were added to the plugin or `ghcide` test suites.
