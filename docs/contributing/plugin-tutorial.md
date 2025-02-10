# Let’s write a Haskell Language Server plugin
Originally written by Pepe Iborra, maintained by the Haskell community.

Haskell Language Server (HLS) is an LSP server for the Haskell programming language. It builds on several previous efforts
to create a Haskell IDE. You can find many more details on the history and architecture in the [IDE 2020](https://mpickering.github.io/ide/index.html) community page.

In this article we are going to cover the creation of an HLS plugin from scratch: a code lens to display explicit import lists.
Along the way we will learn about HLS, its plugin model, and the relationship with `ghcide` and LSP.

## Introduction

Writing plugins for HLS is a joy. Personally, I enjoy the ability to tap into the gigantic bag of goodies that is GHC, as well as the IDE integration thanks to LSP.

In the last couple of months I have written various HLS (and `ghcide`) plugins for things like:

1. Suggest imports for variables not in scope,
2. Remove redundant imports,
2. Evaluate code in comments (à la [doctest](https://docs.python.org/3/library/doctest.html)),
3. Integrate the [retrie](https://github.com/facebookincubator/retrie) refactoring library.

These plugins are small but meaningful steps towards a more polished IDE experience, and in writing them I didn't have to worry about performance, UI, distribution, or even think for the most part, since it's always another tool (usually GHC) doing all the heavy lifting. The plugins also make these tools much more accessible to all users of HLS.

## The task

Here is a visual statement of what we want to accomplish:

   ![Imports code lens](imports.gif)

And here is the gist of the algorithm:

1. Request the type checking artifacts from the `ghcide` subsystem
2. Extract the actual import lists from the type-checked AST,
3. Ask GHC to produce the minimal import lists for this AST,
4. For every import statement without an explicit import list, find out the minimal import list, and produce a code lens to display it together with a command to graft it on.

## Setup

To get started, let’s fetch the HLS repository and build it. You need at least GHC 9.0 for this:

```
git clone --recursive http://github.com/haskell/haskell-language-server hls
cd hls
cabal update
cabal build
```

If you run into any issues trying to build the binaries, the `#haskell-language-server` IRC chat room in
[Libera Chat](https://libera.chat/) is always a good place to ask for help.

Once cabal is done take a note of the location of the `haskell-language-server` binary and point your LSP client to it. In VSCode this is done by editing the "Haskell Server Executable Path" setting. This way you can simply test your changes by reloading your editor after rebuilding the binary.

![Settings](settings-vscode.png)

## Anatomy of a plugin

HLS plugins are values of the `Plugin` datatype, which is defined in `Ide.Plugin` as:
```haskell
data PluginDescriptor =
  PluginDescriptor { pluginId                 :: !PluginId
                   , pluginRules              :: !(Rules ())
                   , pluginCommands           :: ![PluginCommand]
                   , pluginCodeActionProvider :: !(Maybe CodeActionProvider)
                   , pluginCodeLensProvider   :: !(Maybe CodeLensProvider)
                   , pluginHoverProvider      :: !(Maybe HoverProvider)
                   , pluginSymbolsProvider    :: !(Maybe SymbolsProvider)
                   , pluginFormattingProvider :: !(Maybe (FormattingProvider IO))
                   , pluginCompletionProvider :: !(Maybe CompletionProvider)
                   , pluginRenameProvider     :: !(Maybe RenameProvider)
                   }
```
A plugin has a unique ID, a set of rules, a set of command handlers, and a set of "providers":

* Rules add new targets to the Shake build graph defined in `ghcide`. 99% of plugins need not define any new rules.
* Commands are an LSP abstraction for actions initiated by the user which are handled in the server. These actions can be long running and involve multiple modules. Many plugins define command handlers.
* Providers are a query-like abstraction where the LSP client asks the server for information. These queries must be fulfilled as quickly as possible.

The HLS codebase includes several plugins under the namespace `Ide.Plugin.*`, the most relevant are:

- The `ghcide` plugin, which embeds `ghcide` as a plugin (`ghcide` is also the engine under HLS),
- The `ormolu`, `fourmolu`, `floskell` and `stylish-haskell` plugins, a testament to the code formatting wars of our community,
- The `eval` plugin, a code lens provider to evaluate code in comments,
- The `retrie` plugin, a code actions provider to execute retrie commands.

I would recommend looking at the existing plugins for inspiration and reference.

Plugins are "linked" in the `HlsPlugins` module, so we will need to add our plugin there once we have defined it:

```haskell
idePlugins = pluginDescToIdePlugins allPlugins
  where
    allPlugins =
      [ GhcIde.descriptor "ghcide"
      , Pragmas.descriptor "pragmas"
      , Floskell.descriptor "floskell"
      , Fourmolu.descriptor "fourmolu"
      , Ormolu.descriptor "ormolu"
      , StylishHaskell.descriptor "stylish-haskell"
      , Retrie.descriptor "retrie"
      , Eval.descriptor "eval"
      ]
```
To add a new plugin, simply extend the list of `allPlugins` and rebuild.

## Providers

99% of plugins will want to define at least one type of provider. But what is a provider? Let's take a look at some types:
```haskell
type CodeActionProvider =  LSP.LspFuncs Config
                        -> IdeState
                        -> PluginId
                        -> TextDocumentIdentifier
                        -> Range
                        -> CodeActionContext
                        -> IO (Either ResponseError (List CAResult))

type CompletionProvider = LSP.LspFuncs Config
                        -> IdeState
                        -> CompletionParams
                        -> IO (Either ResponseError CompletionResponseResult)

type CodeLensProvider = LSP.LspFuncs Config
                      -> IdeState
                      -> PluginId
                      -> CodeLensParams
                      -> IO (Either ResponseError (List CodeLens))

type RenameProvider = LSP.LspFuncs Config
                    -> IdeState
                    -> RenameParams
                    -> IO (Either ResponseError WorkspaceEdit)
```

Providers are functions that receive some inputs and produce an IO computation that returns either an error or some result.

All providers receive an `LSP.LspFuncs` value, which is a record of functions to perform LSP actions. Most providers can safely ignore this argument, since the LSP interaction is automatically managed by HLS.
Some of its capabilities are:
- Querying the LSP client capabilities,
- Manual progress reporting and cancellation, for plugins that provide long running commands (like the `retrie` plugin),
- Custom user interactions via [message dialogs](https://microsoft.github.io/language-server-protocol/specification#window_showMessage). For instance, the `retrie` plugin uses this to report skipped modules.

The second argument, which plugins receive, is `IdeState`. `IdeState` encapsulates all the `ghcide` state including the build graph. This allows to request `ghcide` rule results, which leverages Shake to parallelize and reuse previous results as appropriate. Rule types are  instances of the `RuleResult` type family, and
most of them are defined in `Development.IDE.Core.RuleTypes`. Some relevant rule types are:
```haskell
-- | The parse tree for the file using GetFileContents
type instance RuleResult GetParsedModule = ParsedModule

-- | The type checked version of this file
type instance RuleResult TypeCheck = TcModuleResult

-- | A GHC session that we reuse.
type instance RuleResult GhcSession = HscEnvEq

-- | A GHC session preloaded with all the dependencies
type instance RuleResult GhcSessionDeps = HscEnvEq

-- | A ModSummary that has enough information to be used to get .hi and .hie files.
type instance RuleResult GetModSummary = ModSummary
```

The `use` family of combinators allows to request rule results. For example, the following code is used in the `eval` plugin to request a GHC session and a module summary (for the imports) in order to set up an interactive evaluation environment
```haskell
  let nfp = toNormalizedFilePath' fp
  session <- runAction "runEvalCmd.ghcSession" state $ use_ GhcSessionDeps nfp
  ms <- runAction "runEvalCmd.getModSummary" state $ use_ GetModSummary nfp
```

There are three flavours of `use` combinators:

1. `use*` combinators block and propagate errors,
2. `useWithStale*` combinators block and switch to stale data in case of an error,
3. `useWithStaleFast*` combinators return immediately with stale data if any, or block otherwise.

## LSP abstractions

If you have used VSCode or any other LSP editor you are probably already familiar with the capabilities afforded by LSP. If not, check the [specification](https://microsoft.github.io/language-server-protocol/specification) for the full details.
Another good source of information is the [haskell-lsp-types](https://hackage.haskell.org/package/haskell-lsp-types) package, which contains a Haskell encoding of the protocol.

The [haskell-lsp-types](https://hackage.haskell.org/package/haskell-lsp-types-0.22.0.0/docs/Language-Haskell-LSP-Types.html#t:CodeLens) package encodes code lenses in Haskell as:
```haskell
data CodeLens =
  CodeLens
    { _range   :: Range
    , _command :: Maybe Command
    , _xdata   :: Maybe A.Value
    } deriving (Read,Show,Eq)
```
That is, a code lens is a triple of a source range, maybe a command, and optionally some extra data. The [specification](https://microsoft.github.io/language-server-protocol/specification#textDocument_codeLens) clarifies the optionality:
```
/**
 * A code lens represents a command that should be shown along with
 * source text, like the number of references, a way to run tests, etc.
 *
 * A code lens is _unresolved_ when no command is associated to it. For performance
 * reasons the creation of a code lens and resolving should be done in two stages.
 */
```

To keep things simple our plugin won't make use of the unresolved facility, embedding the command directly in the code lens.

## The explicit imports plugin

To provide code lenses, our plugin must define a code lens provider as well as a command handler.
The code at `Ide.Plugin.Example` shows how the convenience `defaultPluginDescriptor` function is used
to bootstrap the plugin and how to add the desired providers:

```haskell
descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId) {
    -- This plugin provides code lenses
    pluginCodeLensProvider = Just provider,
    -- This plugin provides a command handler
    pluginCommands = [ importLensCommand ]
}
```

### The command handler

Our plugin provider has two components that need to be fleshed out. Let's start with the command provider, since it's the simplest of the two.

```haskell
importLensCommand :: PluginCommand
```

`PluginCommand` is a data type defined in `LSP.Types` as:

```haskell
data PluginCommand = forall a. (FromJSON a) =>
  PluginCommand { commandId   :: CommandId
                , commandDesc :: T.Text
                , commandFunc :: CommandFunction a
                }
```

The meat is in the `commandFunc` field, which is of type `CommandFunction`, another type synonym from `LSP.Types`:
```haskell
type CommandFunction a =
  LSP.LspFuncs Config
  -> IdeState
  -> a
  -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
```

`CommandFunction` takes in the familiar `LspFuncs` and `IdeState` arguments, together with a JSON encoded argument.
I recommend checking the LSP specifications in order to understand how commands work, but briefly the LSP server (us) initially sends a command descriptor to the client, in this case as part of a code lens. When the client decides to execute the command on behalf of a user action (in this case a click on the code lens), the client sends this descriptor back to the LSP server which then proceeds to handle and execute the command. The latter part is implemented by the `commandFunc` field of our `PluginCommand` value.

For our command, we are going to have a very simple handler that receives a diff (`WorkspaceEdit`) and returns it to the client. The diff will be generated by our code lens provider and sent as part
of the code lens to the LSP client, who will send it back to our command handler when the user activates
the code lens:
```haskell
importCommandId :: CommandId
importCommandId = "ImportLensCommand"

importLensCommand :: PluginCommand
importLensCommand =
    PluginCommand importCommandId "Explicit import command" runImportCommand

-- | The type of the parameters accepted by our command
data ImportCommandParams = ImportCommandParams WorkspaceEdit
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

-- | The actual command handler
runImportCommand :: CommandFunction ImportCommandParams
runImportCommand _lspFuncs _state (ImportCommandParams edit) = do
    return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams edit))

```

### The code lens provider

The code lens provider implements all the steps of the algorithm described earlier:

>  1. Request the type checking artefacts from the `ghcide` subsystem
>  2. Extract the actual import lists from the type-checked AST,
>  3. Ask GHC to produce the minimal import lists for this AST,
>  4. For every import statement without an explicit import list, find out the minimal import list, and produce a code lens to display it together with a command to graft it on.

The provider takes the usual `LspFuncs` and `IdeState` argument, as well as a `CodeLensParams` value containing the URI
for a file, and returns an IO action producing either an error or a list of code lenses for that file.

```haskell
provider :: CodeLensProvider
provider _lspFuncs          -- LSP functions, not used
         state              -- ghcide state, used to retrieve typechecking artifacts
         pId                -- Plugin ID
         CodeLensParams{_textDocument = TextDocumentIdentifier{_uri}}
  -- VSCode uses URIs instead of file paths
  -- haskell-lsp provides conversion functions
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri
  = do
    -- Get the typechecking artifacts from the module
    tmr <- runAction "importLens" state $ use TypeCheck nfp
    -- We also need a GHC session with all the dependencies
    hsc <- runAction "importLens" state $ use GhcSessionDeps nfp
    -- Use the GHC API to extract the "minimal" imports
    (imports, mbMinImports) <- extractMinimalImports hsc tmr

    case mbMinImports of
        Just minImports -> do
            let minImportsMap =
                    Map.fromList [ (srcSpanStart l, i) | L l i <- minImports ]
            lenses <- forM imports $
              -- for every import, maybe generate a code lens
              generateLens pId _uri minImportsMap
            return $ Right (List $ catMaybes lenses)
        _ ->
            return $ Right (List [])
  | otherwise
  = return $ Right (List [])
```

Note how simple it is to retrieve the type checking artifacts for the module as well as a fully setup GHC session via the `ghcide` rules.

The function `extractMinimalImports` extracts the import statements from the AST and generates the minimal import lists, implementing steps 2 and 3 of the algorithm.
The details of the GHC API are not relevant to this tutorial, but the code is terse and easy to read:

```haskell
extractMinimalImports
  :: Maybe HscEnvEq
  -> Maybe TcModuleResult
  -> IO ([LImportDecl GhcRn], Maybe [LImportDecl GhcRn])
extractMinimalImports (Just hsc)) (Just (tmrModule -> TypecheckedModule{..})) = do
    -- Extract the original imports and the typechecking environment
    let (tcEnv,_) = tm_internals_
        Just (_, imports, _, _) = tm_renamed_source
        ParsedModule{ pm_parsed_source = L loc _} = tm_parsed_module
        span = fromMaybe (error "expected real") $ realSpan loc

    -- GHC is secretly full of mutable state
    gblElts <- readIORef (tcg_used_gres tcEnv)

    let usage = findImportUsage imports gblElts
    (_, minimalImports) <-
    -- getMinimalImports computes the minimal explicit import lists
      initTcWithGbl (hscEnv hsc) tcEnv span $ getMinimalImports usage
    return (imports, minimalImports)
extractMinimalImports _ _ = return ([], Nothing)
```

The function `generateLens` implements step 4 of the algorithm, producing a code lens for an import statement that lacks an import list. Note how the code lens includes an `ImportCommandParams` value
that contains a workspace edit that rewrites the import statement, as expected by our command provider.

```haskell
-- | Given an import declaration, generate a code lens unless it has an explicit import list
generateLens :: PluginId
             -> Uri
             -> Map SrcLoc (ImportDecl GhcRn)
             -> LImportDecl GhcRn
             -> IO (Maybe CodeLens)
generateLens pId uri minImports (L src imp)
  -- Explicit import list case
  | ImportDecl{ideclHiding = Just (False,_)} <- imp
  = return Nothing
  -- No explicit import list
  | RealSrcSpan l <- src
  , Just explicit <- Map.lookup (srcSpanStart src) minImports
  , L _ mn <- ideclName imp
  -- (Almost) no one wants to see an explicit import list for Prelude
  , mn /= moduleName pRELUDE
  = do
        -- The title of the command is just the minimal explicit import decl
    let title = T.pack $ prettyPrint explicit
        -- The range of the code lens is the span of the original import decl
        _range :: Range = realSrcSpanToRange l
        -- The code lens has no extra data
        _xdata = Nothing
        -- An edit that replaces the whole declaration with the explicit one
        edit = WorkspaceEdit (Just editsMap) Nothing
        editsMap = HashMap.fromList [(uri, List [importEdit])]
        importEdit = TextEdit _range title
        -- The command argument is simply the edit
        _arguments = Just [toJSON $ ImportCommandParams edit]
    -- Create the command
    _command <- Just <$> mkLspCommand pId importCommandId title _arguments
    -- Create and return the code lens
    return $ Just CodeLens{..}
  | otherwise
  = return Nothing
```

## Wrapping up

There's only one Haskell code change left to do at this point: "link" the plugin in the `HlsPlugins` HLS module.
However integrating the plugin in HLS itself will need some changes in configuration files. The best way is looking for the ID (f.e. `hls-class-plugin`) of an existing plugin:
- `./cabal*.project` and `./stack*.yaml`: add the plugin package in the `packages` field,
- `./haskell-language-server.cabal`: add a conditional block with the plugin package dependency,
- `./.github/workflows/test.yml`: add a block to run the test suite of the plugin,
- `./.github/workflows/hackage.yml`: add the plugin to the component list to release the plugin package to Hackage,
- `./*.nix`: add the plugin to Nix builds.

The full code as used in this tutorial, including imports, can be found in [this Gist](https://gist.github.com/pepeiborra/49b872b2e9ad112f61a3220cdb7db967) as well as in this [branch](https://github.com/pepeiborra/ide/blob/imports-lens/src/Ide/Plugin/ImportLens.hs)

I hope this has given you a taste of how easy and joyful it is to write plugins for HLS.
If you are looking for ideas for contributing, here are some cool ones found in the HLS [issue tracker](https://github.com/haskell/haskell-language-server/issues?q=is%3Aopen+is%3Aissue+label%3A%22type%3A+possible+new+plugin%22).
