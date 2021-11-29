---
name: Bug report
about: Create a report to help us improve
title: ''
labels: 'status: needs triage, type: bug'
assignees: ''

---

<!--
Before opening an issue, please take a look at the [troubleshooting guide](https://haskell-language-server.readthedocs.io/en/latest/troubleshooting.html). This explains some common issues and will also help you to find the information that the issue template asks for.

When filing an issue, please fill out as much of the information below as you can. This helps us to debug your issue, but is not required!
-->

### Your environment

Which OS do you use:
<!-- Windows, MacOS, Ubuntu, ArchLinux, etc... -->
Which lsp-client do you use:
<!-- Neovim, emacs, VS Codium, etc... -->
Describe your project (alternative: link to the project):
<!-- stack.yaml, package.yaml, *.cabal files, cabal.project, hie.yaml -->

### Steps to reproduce
<!-- Tell us how to reproduce this issue. -->

### Expected behaviour
<!-- Tell us what should happen. -->

### Actual behaviour
<!-- Tell us what happens instead. -->

### Include debug information
Execute in the root of your project the command `haskell-language-server-wrapper --debug .` and paste the logs here:
(if you are using the vscode extension check the executable location [here](https://github.com/haskell/vscode-haskell#downloaded-binaries))
<details>
<summary>
Debug output:
</summary>

```
<paste your logs here>
```
</details>

Paste the logs from the lsp-client, you can check instructions about for VS Code [here](https://github.com/haskell/vscode-haskell#troubleshooting)

<details>
<summary>
LSP logs:
</summary>

```
<paste your logs here>
```
</details>
