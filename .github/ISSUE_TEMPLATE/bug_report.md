---
name: Bug report
about: Create a report to help us improve
title: ''
labels: 'status: needs triage, type: bug'
assignees: ''

---

<!--
If you encounter a bug or you have a support question, please try to fill out some of the information below.
Please take a look to the [troubleshooting guide](https://haskell-language-server.readthedocs.io/en/latest/troubleshooting.html) before filling this new one.
The information below is meant to help debugging issues but is no prerequisite for opening an issue.
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
Execute in the root of your project the command `haskell-language-server-wrapper --debug .` or `haskell-language-server --debug .` and paste the logs here:

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
