# Change Type Signature Plugin

The change type signature plugin provides a code action to change a user's current type signature to it's actual type signature.
The plugin does not work in all error scenarios. Currently, the plugin uses GHC diagnostic messages to recover the actual type of a function.
If the plugin receives enough information it can correctly change the signature.

## Demo

![Change Type Signature One](change1.gif)

![Change Type Signature Two](change2.gif)


## Changelog
### 1.0.0.0
- First Release

### 1.0.1.0
- Fix 9.2 Test failures (`waitForProgressDone`)
- Add extra test scenarios for error message diffs in 9.2
- Remove regex parsing for simple `Text` manipulation
