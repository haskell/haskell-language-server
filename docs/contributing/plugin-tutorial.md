# Let’s write a Haskell Language Server plugin

Haskell Language Server is an LSP server for the Haskell programming language. It builds on several previous efforts
to create a Haskell IDE, you can find many more details on the history and architecture in the [IDE 2020](https://mpickering.github.io/ide/index.html) community page.

In this article we are going to cover the creation of an HLS plugin from scratch: a code lens to display explicit import lists.
Along the way we will learn about HLS, its plugin model, and the relationship with [ghcide](https://github.com/haskell/haskell-language-server/tree/master/ghcide) and LSP.

## Introduction

Writing plugins for HLS is a joy. Personally, I enjoy the ability to tap into the gigantic bag of goodies that is GHC, as well as the IDE integration thanks to LSP.

In the last couple of months I have written various HLS plugins for things like:

1. Suggest imports for variables not in scope,
2. Remove redundant imports,
2. Evaluate code in comments (a la doctest),
3. Integrate the [retrie](https://github.com/facebookincubator/retrie) refactoring library.

These plugins are small but meaningful steps towards a more polished IDE experience, and when writing them I didn't have to worry about performance, UI, distribution, or even think for the most part, since it's always another tool (usually GHC) doing all the heavy lifting. The plugins also make these tools much more accessible to all the users of HLS.

## Plugins in the HLS codebase

The `haskell-language-server` codebase includes several plugins (found in `./plugins`). Notable examples include:

- The `ormolu`, `fourmolu`, `floskell` and `stylish-haskell` plugins
- The `eval` plugin, a code lens provider to evaluate code in comments
- The `retrie` plugin, a code actions provider to execute retrie commands

I would recommend looking at the existing plugins for inspiration and reference. A few conventions shared by all plugins:

- Plugins are located in the `./plugins` folder
- Plugins implement their code under the `Ide.Plugin.*` namespace
- Folders containing the plugin follow the `hls-pluginname-plugin` naming convention
- Plugins are "linked" in `src/HlsPlugins.hs#idePlugins`, so new plugin descriptors must be added there:
    ```haskell
    -- src/HlsPlugins.hs

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
          , NewPlugin.descriptor "new-plugin" -- New plugin added here
          ]
    ```
To add a new plugin, extend the list of `allPlugins` and rebuild.

## The goal of the plugin

Here is a visual statement of what we want to accomplish:

   ![Imports code lens](imports.gif)

And here is the gist of the algorithm:

1. Request the type checking artefacts
2. Extract the actual import lists from the type checked AST,
3. Ask GHC to produce the minimal import lists for this AST,
4. For every import statement without a explicit import list:
   - find out the minimal import list
   - produce a code lens to display it and a command to apply it

## Setup

To get started, let’s fetch the HLS repo and build it by following the [setup instructions](https://haskell-language-server.readthedocs.io/en/latest/contributing/contributing.html#building) in the Contributing section of this documentation.

If you run into any issues trying to build the binaries, you can get in touch with the HLS team using one of the [contact channels](https://haskell-language-server.readthedocs.io/en/latest/contributing/contributing.html#how-to-contact-the-haskell-ide-team) or [open an issue](https://github.com/haskell/haskell-language-server/issues) in the HLS repository.

Make sure you use the HLS package you just built by following [this section](https://haskell-language-server.readthedocs.io/en/latest/contributing/contributing.html#manually-testing-your-hacked-hls) of the "Contributing" guide.

## Anatomy of a plugin

HLS plugins are values of the `PluginDescriptor` datatype, which is defined in `hls-plugin-api/src/Ide/Types.hs` as:
```haskell
data PluginDescriptor (ideState :: *) =
  PluginDescriptor { pluginId                   :: PluginId
                   , pluginRules                :: Rules ()
                   , pluginCommands             :: [PluginCommand ideState]
                   , pluginHandlers             :: PluginHandlers ideState
                   , pluginNotificationHandlers :: PluginNotificationHandlers ideState
                   , [...] -- Other fields omitted for brevity.
                   }
```
A plugin has a unique id, command handlers, request handlers, notification handlers and rules:

* Request handlers are called when an LSP client asks the server for information. These queries must be fulfilled as quickly as possible.
* Notification handlers are called by code that was not directly triggerd by an user/client.
* Rules add new targets to the Shake build graph. Most plugins do not need to define new rules.
* Commands are an LSP abstraction for actions initiated by the user which are handled in the server. These actions can be long running and involve multiple modules.

## The explicit imports plugin

To achieve the plugin goals, we will have to define:
- a command handler (`importLensCommand`)
- a code lens request handler (`lensProvider`)

These will be assembled together in the `descriptor` function of the plugin, which contains all the information wrapped in the `PluginDescriptor` datatype mentioned above.

Using the convenience `defaultPluginDescriptor` function, the plugin can be bootstrapped with the required parts:

```haskell
-- plugins/hls-explicit-imports-plugin/src/Ide/Plugin/ExplicitImports.hs

-- | The "main" function of a plugin
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId)
    { pluginCommands = [importLensCommand], -- The plugin provides a command handler
      pluginHandlers = mconcat -- The plugin provides request handlers
        [ lensProvider
        ]
    }
```

We'll start with the command, since it's the simplest of the two.

### The command handler

In short, commands works like this:
- The LSP server (HLS) initially sends a command descriptor to the client, in this case as part of a code lens.
- Whenever the client decides to execute the command on behalf of a user (in this case a click on the code lens), it sends this same descriptor back to the LSP server which then proceeds to handle and execute the command. The latter part is implemented by the `commandFunc` field of our `PluginCommand` value.

> **Note**: Check the [LSP spec](https://microsoft.github.io/language-server-protocol/specification) for a deeper understanding of how commands work.

The command handler will be called `importLensCommand` and have the `PluginCommand` type, which is a type defined in `Ide.Types` as:

```haskell
-- hls-plugin-api/src/Ide/Types.hs

data PluginCommand ideState = forall a. (FromJSON a) =>
  PluginCommand { commandId   :: CommandId
                , commandDesc :: T.Text
                , commandFunc :: CommandFunction ideState a
                }
```

Let's start by creating an unfinished command handler. We'll give it an id and a description for now:

```haskell
-- | The command handler
importLensCommand :: PluginCommand IdeState
importLensCommand =
  PluginCommand
    { commandId = "ImportLensCommand"
    , commandDesc = "Explicit import command"
    , commandFunc = runImportCommand
    }

-- | Not implemented yet.
runImportCommand = undefined
```

The most important (and still `undefined`) field is `commandFunc :: CommandFunction`, a type synonym from `LSP.Types`:

```haskell
-- hls-plugin-api/src/Ide/Types.hs

type CommandFunction ideState a
  = ideState
  -> a
  -> LspM Config (Either ResponseError Value)
```

`CommandFunction` takes in an `ideState` and a JSON-encodable argument.

Our handler will ignore the state argument and only really use the `WorkspaceEdit` argument.

```haskell
-- | The type of the parameters accepted by our command
newtype ImportCommandParams = ImportCommandParams WorkspaceEdit
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The actual command handler
runImportCommand :: CommandFunction IdeState ImportCommandParams
runImportCommand _ (ImportCommandParams edit) = do
  -- This command simply triggers a workspace edit!
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  return (Right Null)
```

It [sends a request](https://hackage.haskell.org/package/lsp-1.6.0.0/docs/Language-LSP-Server.html#v:sendRequest) with the method `SWorkspaceApplyEdit` to the client with the `ApplyWorkspaceEditParams Nothing edit` parameters and a response handler (that does nothing). It then returns `Right Null`, an empty `Aeson.Value` wrapped in `Right`.

### The code lens provider

The code lens provider implements all the steps of the algorithm described earlier:

>  1. Request the type checking artefacts
>  2. Extract the actual import lists from the type checked AST,
>  3. Ask GHC to produce the minimal import lists for this AST,
>  4. For every import statement without a explicit import list, find out what's the minimal import list, and produce a code lens to display it together with a diff to graft the import list in.

The provider takes the usual `LspFuncs` and `IdeState` argument, as well as a `CodeLensParams` value containing the URI
for a file, and returns an IO action producing either an error or a list of code lenses for that file.

```haskell
provider :: CodeLensProvider
provider _lspFuncs          -- LSP functions, not used
         state              -- ghcide state, used to retrieve typechecking artifacts
         pId                -- plugin Id
         CodeLensParams{_textDocument = TextDocumentIdentifier{_uri}}
  -- VSCode uses URIs instead of file paths
  -- haskell-lsp provides conversion functions
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri
  = do
    -- Get the typechecking artifacts from the module
    tmr <- runAction "importLens" state $ use TypeCheck nfp
    -- We also need a GHC session with all the dependencies
    hsc <- runAction "importLens" state $ use GhcSessionDeps nfp
    -- Use the GHC api to extract the "minimal" imports
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

Note how simple it is to retrieve the type checking artifacts for the module as well as a fully setup Ghc session via the Ghcide rules.

The function `extractMinimalImports` extracts the import statements from the AST and generates the minimal import lists, implementing steps 2 and 3 of the algorithm.
The details of the GHC api are not relevant to this tutorial, but the code is terse and easy to read:

```haskell
extractMinimalImports
  :: Maybe HscEnvEq
  -> Maybe TcModuleResult
  -> IO ([LImportDecl GhcRn], Maybe [LImportDecl GhcRn])
extractMinimalImports (Just hsc)) (Just (tmrModule -> TypecheckedModule{..})) = do
    -- extract the original imports and the typechecking environment
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

The function `generateLens` implements the last piece of the algorithm, step 4, producing a code lens for an import statement that lacks an import list. Note how the code lens includes an `ImportCommandParams` value
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
  -- (almost) no one wants to see an explicit import list for Prelude
  , mn /= moduleName pRELUDE
  = do
        -- The title of the command is just the minimal explicit import decl
    let title = T.pack $ prettyPrint explicit
        -- the range of the code lens is the span of the original import decl
        _range :: Range = realSrcSpanToRange l
        -- the code lens has no extra data
        _xdata = Nothing
        -- an edit that replaces the whole declaration with the explicit one
        edit = WorkspaceEdit (Just editsMap) Nothing
        editsMap = HashMap.fromList [(uri, List [importEdit])]
        importEdit = TextEdit _range title
        -- the command argument is simply the edit
        _arguments = Just [toJSON $ ImportCommandParams edit]
    -- create the command
    _command <- Just <$> mkLspCommand pId importCommandId title _arguments
    -- create and return the code lens
    return $ Just CodeLens{..}
  | otherwise
  = return Nothing
```

## Wrapping up

There's only one haskell code change left to do at this point: "link" the plugin in the `HlsPlugins` HLS module.
However integrating the plugin in haskell-language-server itself will need some changes in config files. The best way is looking for the id (f.e. `hls-tactics-plugin`) of an existing plugin:
- `./cabal*.project` and `./stack*.yaml`: add the plugin package in the `packages` field
- `./haskell-language-server.cabal`: add a conditional block with the plugin package dependency
- `./.github/workflows/test.yml`: add a block to run the test suite of the plugin
- `./.github/workflows/hackage.yml`: add the plugin to the component list to release the plugin package to hackage
- `./*.nix`: add the plugin to nix builds

The full code as used in this tutorial, including imports, can be found in [this Gist](https://gist.github.com/pepeiborra/49b872b2e9ad112f61a3220cdb7db967) as well as in this [branch](https://github.com/pepeiborra/ide/blob/imports-lens/src/Ide/Plugin/ImportLens.hs)

I hope this has given you a taste of how easy and joyful it is to write plugins for HLS.
If you are looking for ideas for contributing, here are some cool ones found in the HLS [issue tracker](https://github.com/haskell/haskell-language-server/issues?q=is%3Aopen+is%3Aissue+label%3A%22type%3A+possible+new+plugin%22).

TODO: Figure out what to do with the following sections:

## Further info

### LSP abstractions

If you have used VSCode or any other LSP editor you are probably already familiar with the capabilities afforded by LSP. If not, check the [specification](https://microsoft.github.io/language-server-protocol/specification) for the full details.
Another good source of information is the [haskell-lsp-types](https://hackage.haskell.org/package/haskell-lsp-types) package, which contains a Haskell encoding of the protocol.

The [haskell-lsp-types](https://hackage.haskell.org/package/lsp-types) package encodes code lenses in Haskell as:
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

### Providers
**DEPRECATED**: _Providers were split into request handlers and notification handlers. The following section might contain outdated information._

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
- Querying the LSP client capabilities
- Manual progress reporting and cancellation, for plugins that provide long running commands (like the Retrie plugin),
- Custom user interactions via [message dialogs](https://microsoft.github.io/language-server-protocol/specification#window_showMessage). For instance, the Retrie plugin uses this to report skipped modules.

The second argument plugins receive is `IdeState`, which encapsulates all the ghcide state including the build graph. This allows to request ghcide rule results, which leverages Shake to parallelize and reuse previous results as appropriate. Rule types are  instances of the `RuleResult` type family, and
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

The `use` family of combinators allow to request rule results. For example, the following code is used in the Eval plugin to request a GHC session and a module summary (for the imports) in order to set up an interactive evaluation environment
```haskell
  let nfp = toNormalizedFilePath' fp
  session <- runAction "runEvalCmd.ghcSession" state $ use_ GhcSessionDeps nfp
  ms <- runAction "runEvalCmd.getModSummary" state $ use_ GetModSummary nfp
```

There are three flavours of `use` combinators:

1. `use*` combinators block and propagate errors,
2. `useWithStale*` combinators block and switch to stale data in case of error,
3. `useWithStaleFast*` combinators return immediately with stale data if any, or block otherwise.

