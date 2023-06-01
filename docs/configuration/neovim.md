# Detailed Neovim Configuration

This document is intended to give detailed instructions for how to get haskell language server up and running for those who don't know much about how to configure neovim with lua. These instructions should be complete enough to get neovim working with hls even if you are starting from scratch. This guide will set up a working configuration for [haskell-tools.nvim](https://github.com/MrcJkb/haskell-tools.nvim).

## Installing Neovim

haskell-tools.nvim requires neovim >= 0.8. Neovim can be installed many ways. One that has worked for us is [asdf](https://asdf-vm.com/), which we will also use to install lua. The [install instructions for asdf](https://asdf-vm.com/guide/getting-started.html) are detailed and include the differences for different shells and operating systems, including macOS. Once you have followed the asdf installation instructions, please ensure that `~/.asdf/bin` and `~/.asdf/shims` are in your $PATH.
Next you need to install the [neovim plugin for asdf](https://github.com/richin13/asdf-neovimhttps://github.com/richin13/asdf-neovim):
```
asdf plugin add neovim
```
With this plugin you can see different versions of neovim that are available for installation with
```
asdf list all neovim
```
Then install one of the versions that is greater than or equal to version 0.8, for example
```
asdf install neovim 0.9.0
```
Now you should have a working installation of neovim that is compatible with haskell-tools.nvim.

## Installing Lua and Luarocks

Just as with neovim, we will install lua using asdf. Lua is the configuration language that haskell-tools.nvim and its dependencies use to configure neovim. Luarocks is the package manager for lua and is the tool we will use for installing haskell-tools.nvim and its dependencies.
First, install the [lua asdf plugin](https://github.com/Stratus3D/asdf-lua):
```
asdf plugin-add lua https://github.com/Stratus3D/asdf-lua.git
```
As with neovim, you can see the versions of lua available with
```
asdf list all lua
```
You should install the last version of lua 5.1, as [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim) (one of the dependencies of haskell-tools.nvim) is only compatible with lua 5.1.
```
asdf install lua 5.1.5
```
Installing lua in this fashion also installs luarocks automatically.
To ensure that neovim can see the plugins we will install with luarocks, we need to set the LUA_PATH environment variable with the location of those plugins. The necessary paths should be
```
$HOME/.asdf/installs/lua/5.1.5/luarocks/share/lua/5.1/?.lua
$HOME/.asdf/installs/lua/5.1.5/luarocks/share/lua/5.1/?/init.lua
```
Setting environment variables varies somewhat for different shells; just make sure that the LUA_PATH variable contains the paths above.
To check that you have the correct values for your LUA_PATH, you can run
```
luarocks path
```
and the paths under `$HOME/.asdf` where plugins are installed should be included in that output.

## Installing haskell-tools.nvim and its dependencies
We will use luarocks to install the neovim plugins that we need.
```
luarocks install plenary.nvim
luarocks install telescope.nvim
luarocks install nvim-dap
luarocks install nvim-lspconfig
luarocks install haskell-tools.nvim
```

## Installing the Haskell dependencies
Most importantly, we must install haskell language server! First install [ghcup](https://www.haskell.org/ghcup/):
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
Then use ghcup to install hls:
```
ghcup install hls recommended
ghcup set hls recommended
```
If you haven't already, you should also use ghcup to install ghc, cabal, and stack. For these installations to work ensure that `$HOME/.ghcup/bin` is in your PATH.
Next install [hoogle](https://github.com/ndmitchell/hoogle/blob/master/docs/Install.md), [fast-tags](https://github.com/elaforge/fast-tags), and [haskell-debug-adapter](https://github.com/phoityne/haskell-debug-adapter/):
```
# In some directory where you put open source code
git clone https://github.com/ndmitchell/hoogle.git
cd hoogle
cabal install
cd ..
git clone https://github.com/elaforge/fast-tags.git
cd fast-tags
cabal install
cd ..
stack install haskell-dap ghci-dap haskell-debug-adapter
```

# Configuring Neovim
With all the dependencies installed, we are now ready to add a minimal lua configuration for neovim and haskell-tools.nvim. Create the file `$HOME/.config/nvim/ftplugin/haskell.lua` with the following contents:
```
local ht = require('haskell-tools')
local def_opts = { noremap = true, silent = true, }
ht.start_or_attach {
  hls = {
    on_attach = function(client, bufnr)
      local opts = vim.tbl_extend('keep', def_opts, { buffer = bufnr, })
      -- haskell-language-server relies heavily on codeLenses,
      -- so auto-refresh (see advanced configuration) is enabled by default
      vim.keymap.set('n', '<space>ca', vim.lsp.codelens.run, opts)
      vim.keymap.set('n', '<space>hs', ht.hoogle.hoogle_signature, opts)
      vim.keymap.set('n', '<space>ea', ht.lsp.buf_eval_all, opts)

      --copied from https://github.com/neovim/nvim-lspconfig#suggested-configuration
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
      vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
      vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
      vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, opts)
      vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
      vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
      vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
      vim.keymap.set('n', '<space>f', function()
        vim.lsp.buf.format { async = true }
      end, opts)
    end,
  },
}

-- Suggested keymaps that do not depend on haskell-language-server:
local bufnr = vim.api.nvim_get_current_buf()
-- set buffer = bufnr in ftplugin/haskell.lua
local opts = { noremap = true, silent = true, buffer = bufnr }

-- Toggle a GHCi repl for the current package
vim.keymap.set('n', '<leader>rr', ht.repl.toggle, opts)
-- Toggle a GHCi repl for the current buffer
vim.keymap.set('n', '<leader>rf', function()
  ht.repl.toggle(vim.api.nvim_buf_get_name(0))
end, def_opts)
vim.keymap.set('n', '<leader>rq', ht.repl.quit, opts)

-- Detect nvim-dap launch configurations
-- (requires nvim-dap and haskell-debug-adapter)
ht.dap.discover_configurations(bufnr)
```
That's it! With this configuration, you should be ready to open .hs files with neovim and use the features of haskell language server. Happy hacking!
