# What is the Haskell Language Server?

The Haskell Language Server (HLS) is an implementation of a server (a "language server") for the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).
A language server talks to a client (typically an editor), which can ask the server to perform various operations, such as reporting errors or providing code completions.
The advantage of this system is that clients and servers can interoperate more easily so long as they all speak the LSP protocol.
In the case of HLS, that means that it can be used with many different editors, since editor support for the LSP protocol is now widespread.

## Language Server Protocol

### Servers and clients

HLS is responsible for actually understanding your project and answering the questions that the client asks of it, such as: what completion items could go here? are there any errors in the project? and so on.
HLS provides [many](./features.md) (but not all) of the features that the LSP protocol supports.

But HLS only provides the server part of the setup.
In order to actually use it you also need a client (editor).
The client is responsible for managing your interaction with the server: launching it, dispatching commands to it, and displaying or implementing responses.
Some clients will even install the server binaries for you!

Common clients include:
- VSCode (the reference implementation for a LSP client)
- Emacs, with the `lsp-mode`+`lsp-haskell` or `eglot` packages
- Vim/neovim, with the builtin LSP support or `coc.vim`
- Kate
- ... and more every day!

### LSP terminology

Here are a few pieces of jargon that you may come across in the HLS docs or when discussing problems:

- *Code action*: A code action is a specific action triggered by a user on a particular region of code. Examples might include "add a type signature to this function".
- *Code lens*: A pre-rendered edit or action shown in the body of the document itself, usually triggered with a click. Examples might include "the type signature for a function, which is actually inserted on click".
- *Completion item*: An item that can be inserted into the text, including its metadata.
- *Diagnostic*: Any information about the project that is shown in the editor, including errors, warnings, and hints from tools such as hlint.
- *Semantic highlighting*: Special syntax highlighting performed by the server.
- *Method*: A LSP method is a function in the LSP protocol that the client can invoke to perform some action, e.g. ask for completions at a point.

## Haskell Language Server

### HLS and its wrapper

HLS is a binary that must be compiled with the same version of GHC as the project you are using it on.
For this reason it is usually distributed as a _collection_ of binaries, along with a `haskell-language-server-wrapper` executable that selects the correct one based on which version of GHC it thinks you are using.

In general you can use `haskell-language-server-wrapper` wherever you need to run `haskell-language-server`.

### HLS plugins

HLS has a plugin architecture, whereby individual pieces of functionality are provided by smaller packages that just do one thing.
Plugins can also be disabled independently to allow users to customize the behaviour of HLS to their liking.

These plugins all (currently) live in the HLS repository and are developed in tandem with the core HLS functionality.

See the [configuration page](./configuration.md#Generic plugin configuration) for more on configuring plugins.

### hie-bios

HLS needs to know how to build your Haskell project: what flags to pass, what packages to provide, etc.
It gets this information from the build system used by your project (typically `cabal` or `stack`).
The tool used to do this is called [`hie-bios`](https://github.com/haskell/hie-bios).
`hie-bios` calls the strategy it uses to get compilation flags (e.g. "ask `cabal`") a "cradle".

See the [configuration page](./configuration.md#configuring-your-project-build) for more on configuring cradles.
