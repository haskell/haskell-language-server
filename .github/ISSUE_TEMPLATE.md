If you encounter a bug or you have a support question, please try to fill out some of the information below.
However, if you think your issue does not need any of it, you may omit it.
Generally speaking, the information below is meant for helping debugging issues
but they are no prerequisite for opening issues.

### Subject of the issue
Describe your issue here.

### Your environment
* Output of `haskell-language-server --probe-tools` or `haskell-language-server-wrapper --probe-tools`
  * This command is available since version `>= 0.4.0.0`
* Which lsp-client do you use
  * Neovim, emacs, VS Codium, etc...
* Describe your project (alternative: link to the project)
  * Include `stack.yaml`
  * Include `package.yaml`
  * Include `*.cabal` files
  * Include `cabal.project`
* Contents of `hie.yaml`

### Steps to reproduce
Tell us how to reproduce this issue.

### Expected behaviour
Tell us what should happen.

### Actual behaviour
Tell us what happens instead.

### Include debug information
Execute in the root of your project the command `haskell-language-server --debug .` and paste the logs here:

<details>
<summary>
Debug output:
</summary>

```
<paste your logs here>
```
</details>

Paste the logs from the lsp-client, e.g. for [VS Code](https://github.com/haskell/vscode-haskell#troubleshooting)

<details>
<summary>
LSP logs:
</summary>

```
<paste your logs here>
```
</details>
