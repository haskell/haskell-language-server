# Haskell IDE Core

Our vision is that you should build an IDE by combining:

* [hie-bios](https://github.com/mpickering/haskell-ide-engine/tree/hie-bios/hie-bios) for determining where your files are, what the dependencies, what extensions are enabled etc.
* `haskell-ide-core` - this library - for defining how to type check, when to type check, and producing messages.
* `haskell-lsp` for sending those messages to an LSP server.
* A VS Code extension, e.g. `extension` in this directory.

There are more details [in this blog post](https://4ta.uk/p/shaking-up-the-ide).

## How to use it

Let's assume you want to load the `haskell-ide-core` source code in a VS Code IDE.

1. `git clone https://github.com/digital-asset/daml.git`
2. `cd compiler/haskell-ide-core`
3. `stack build`
4. `cd extension`
5. `npm install`
6. `code .`
7. Press F5 to start the extension.
8. In the spawned extension, open the folder `haskell-ide-core`.
9. In the preferences, set the Haskell IDE Core executable preference to `stack` and the arguments to `exec -- ide-demo --ide .ghci`
10. Run the Reload Window command in VS Code.

Now you should have a working IDE dealing with itself.
