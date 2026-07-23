{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | A plugin inspired by the REPLoid feature of
-- [Dante](https://github.com/jyp/dante),
-- [Haddock examples and properties](https://haskell-haddock.readthedocs.io/latest/markup.html#examples),
-- and [Doctest](https://hackage.haskell.org/package/doctest).
module Ide.Plugin.Eval.Handlers (
    codeAction,
    codeLens,
    evalCommand,
) where

import           Control.Applicative                          (Alternative ((<|>)))
import           Control.Arrow                                (second)
import           Control.Exception                            (bracket_)
import qualified Control.Exception                            as E
import           Control.Lens                                 (ix, (%~), (^.))
import           Control.Monad                                (guard, void,
                                                               when)
import           Control.Monad.IO.Class                       (MonadIO (liftIO))
import           Control.Monad.Trans.Except                   (ExceptT (..),
                                                               runExceptT)
import           Data.Aeson                                   (toJSON)
import           Data.Char                                    (isSpace)
import           Data.Foldable                                (toList)
import           Data.List                                    (dropWhileEnd,
                                                               find,
                                                               intercalate,
                                                               intersperse)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (catMaybes,
                                                               mapMaybe)
import           Data.String                                  (IsString)
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import qualified Data.Text.Utf16.Rope.Mixed                   as Rope
import           Development.IDE.Core.FileStore               (getUriContents, setSomethingModified)
import           Development.IDE.Core.Rules                   (IdeState,
                                                               runAction)
import           Development.IDE.Core.RuleInput               (toProjectHaskellInput, ProjectHaskellInput, IsFileInput (inputFilePath))
import           Development.IDE.Core.Shake                   (use_, uses_, VFSModified (VFSUnmodified), useWithSeparateFingerprintRule_)
import           Development.IDE.GHC.Compat                   hiding (typeKind,
                                                               unitState)
import           Development.IDE.GHC.Compat.Util              (OverridingBool (..))
import           Development.IDE.GHC.Util                     (evalGhcEnv,
                                                               modifyDynFlags)
import           Development.IDE.Import.DependencyInformation (transitiveDeps,
                                                               transitiveModuleDeps)
import           Development.IDE.Types.Location               (toNormalizedFilePath')
import           GHC                                          (ClsInst,
                                                               ExecOptions (execLineNumber, execSourceFile),
                                                               FamInst,
                                                               GhcMonad,
                                                               NamedThing (getName),
                                                               defaultFixity,
                                                               execOptions,
                                                               exprType,
                                                               getInfo,
                                                               getInteractiveDynFlags,
                                                               isImport, isStmt,
                                                               parseName,
                                                               pprFamInst,
                                                               pprInstance,
                                                               typeKind)


import           Development.IDE.Core.RuleTypes               (GetLinkable (GetLinkable),
                                                               GetModSummary (GetModSummary),
                                                               GetModuleGraphTransDepsFingerprints (GetModuleGraphTransDepsFingerprints),
                                                               GhcSessionDeps (GhcSessionDeps),
                                                               ModSummaryResult (msrModSummary),
                                                               LinkableResult (linkableHomeMod),
                                                               TypeCheck (..),
                                                               tmrTypechecked, GetModuleGraphTransDepsFingerprints(..), GetModuleGraph(..))
import qualified Development.IDE.GHC.Compat.Core              as Compat (InteractiveImport (IIModule))
import qualified Development.IDE.GHC.Compat.Core              as SrcLoc (unLoc)
import           Development.IDE.Types.HscEnvEq               (HscEnvEq (hscEnv))
import qualified GHC.LanguageExtensions.Type                  as LangExt (Extension (..))
import           Development.IDE.Session.Ghc                  (disableOptimisation)

import           Data.List.Extra                              (unsnoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Types.Shake                  (toKey)
import           GHC.Types.SrcLoc                             (UnhelpfulSpanReason (UnhelpfulInteractive))
#if MIN_VERSION_ghc(9,13,0)
import           GHC.Types.Avail                              (DetOrdAvails (DefinitelyDeterministicAvails),
                                                               sortAvails)
import           GHC.Types.Name.Set                           (nameSetElemsStable)
#endif
import           Ide.Logger                                   (Priority (..),
                                                               Recorder,
                                                               WithPriority,
                                                               logWith)
import           Ide.Plugin.Error                             (PluginError (PluginInternalError),
                                                               handleMaybeM)
import           Ide.Plugin.Eval.Code                         (Statement,
                                                               asStatements,
                                                               execStmtCaptureResult,
                                                               propSetup,
                                                               resultRange,
                                                               evalExprCheck,
                                                               evalExprRanges)
import           Ide.Plugin.Eval.Config                       (EvalConfig (..),
                                                               getEvalConfig)
import           Ide.Plugin.Eval.GHC                          (addImport,
                                                               addPackages,
                                                               hasPackage,
                                                               setSessionAndInteractiveDynFlags)
import           Ide.Plugin.Eval.Parse.Comments               (commentsToSections)
import           Ide.Plugin.Eval.Parse.Option                 (parseSetFlags)
import           Ide.Plugin.Eval.Rules                        (queueForEvaluation,
                                                               unqueueForEvaluation)
import           Ide.Plugin.Eval.Types
import           Ide.Plugin.Eval.Util                         (gStrictTry,
                                                               isLiterate,
                                                               prettyWarnings,
                                                               response', timed)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens                   as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
#if MIN_VERSION_ghc(9,11,0)
import           GHC.Unit.Module.ModIface                     (IfaceTopEnv (..))
#endif

codeAction :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeAction recorder st plId CodeActionParams{_textDocument,_range} = do
    rangeCommands <- mkRangeCommands recorder st plId _textDocument
    pure
        $ InL
            [ InL command
            | (evalExprRange, command) <- rangeCommands
            , _range `isSubrangeOf` evalExprRange
            ]

{- | Code Lens provider
 NOTE: Invoked every time the document is modified, not just when the document is saved.
-}
codeLens :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentCodeLens
codeLens recorder st plId CodeLensParams{_textDocument} = do
    rangeCommands <- mkRangeCommands recorder st plId _textDocument
    pure
        $ InL
            [ CodeLens range (Just command) Nothing
            | (range, command) <- rangeCommands
            ]

-- | Find every eval-expr in the document and pair its source range with the
-- 'Command' that evaluates it. Shared by the code action and code lens
-- providers.
mkRangeCommands :: Recorder (WithPriority Log) -> IdeState -> PluginId -> TextDocumentIdentifier -> ExceptT PluginError (HandlerM Config) [(Range, Command)]
mkRangeCommands recorder st plId textDocument =
    let dbg = logWith recorder Debug
        perf = timed (\lbl duration -> dbg $ LogExecutionTime lbl duration)
     in perf "evalMkRangeCommands" $
            do
                let TextDocumentIdentifier uri = textDocument
                fp <- uriToFilePathE uri
                let nfp = toNormalizedFilePath' fp
                    isLHS = isLiterate fp
                dbg $ LogCodeLensFp fp
                (comments, _) <-
                    runActionE "eval.GetParsedModuleWithComments" st $ useWithStaleE GetEvalComments nfp
                dbg $ LogCodeLensComments comments

                -- Extract 'EvalExpr's from source code
                let Sections{..} = commentsToSections isLHS comments
                    evalExprs = evalExprsBySection nonSetupSections
                    cmd = mkLspCommand plId evalCommandName "Evaluate=..." (Just [])
                let rangeCommands =
                        [ (evalExprRange, cmd')
                        | (section, ident, evalExpr) <- evalExprs
                        , let (evalExprRange, resultRange) = evalExprRanges evalExpr
                              args = EvalParams (setupSections ++ [section]) textDocument ident
                              cmd' =
                                (cmd :: Command)
                                    { _arguments = Just [toJSON args]
                                    , _title =
                                        if trivial resultRange
                                            then "Evaluate..."
                                            else "Refresh..."
                                    }
                        ]

                perf "evalExprs" $
                    dbg $ LogEvalExprs
                            (length evalExprs)
                            (length nonSetupSections)
                            (length setupSections)
                            (length rangeCommands)

                pure rangeCommands
  where
    trivial (Range p p') = p == p'

evalCommandName :: CommandId
evalCommandName = "evalCommand"

evalCommand :: Recorder (WithPriority Log) -> PluginId -> PluginCommand IdeState
evalCommand recorder plId = PluginCommand evalCommandName "evaluate" (runEvalCmd recorder plId)

type EvalId = Int

runEvalCmd :: Recorder (WithPriority Log) -> PluginId -> CommandFunction IdeState EvalParams
runEvalCmd recorder plId st mtoken EvalParams{..} =
    let dbg = logWith recorder Debug
        perf = timed (\lbl duration -> dbg $ LogExecutionTime lbl duration)
        cmd :: ExceptT PluginError (HandlerM Config) WorkspaceEdit
        cmd = do
            let evalExprs = map (\(a,_,b) -> (a,b)) $ evalExprsBySection sections

            let TextDocumentIdentifier{_uri} = module_
            fp <- uriToFilePathE _uri
            let nfp = toNormalizedFilePath' fp
            mdlText <- moduleText st _uri

            -- enable codegen for the module which we need to evaluate.
            final_hscEnv <- liftIO $ bracket_
              (setSomethingModified VFSUnmodified st "Eval" $ do
                queueForEvaluation st nfp
                return [toKey IsEvaluating nfp]
                )
              (setSomethingModified VFSUnmodified st "Eval" $ do
                unqueueForEvaluation st nfp
                return [toKey IsEvaluating nfp]
                )
              (initialiseSessionForEval (needsQuickCheck evalExprs) st nfp)

            evalCfg <- liftIO $ runAction "eval: config" st $ getEvalConfig plId

            -- Perform the evaluation of the command
            edits <-
                perf "edits" $
                    liftIO $
                        evalGhcEnv final_hscEnv $ do
                            runEvalExprs recorder evalCfg fp evalExprs

            let workspaceEditsMap = Map.singleton _uri (addFinalReturn mdlText edits)
            let workspaceEdits = WorkspaceEdit (Just workspaceEditsMap) Nothing Nothing

            return workspaceEdits
     in perf "evalCmd" $ ExceptT $
            pluginWithIndefiniteProgress "Evaluating" mtoken Cancellable $ \_updater ->
                runExceptT $ response' cmd

-- | Create an HscEnv which is suitable for performing interactive evaluation.
-- All necessary home modules will have linkables and the current module will
-- also be loaded into the environment.
--
-- The interactive context and interactive dynamic flags are also set appropiately.
initialiseSessionForEval :: Bool -> IdeState -> ProjectHaskellInput -> IO HscEnv
initialiseSessionForEval needs_quickcheck st projectFile = do
  (ms, env1) <- runAction "runEvalCmd" st $ do

    ms <- msrModSummary <$> use_ GetModSummary projectFile
    deps_hsc <- hscEnv <$> use_ GhcSessionDeps projectFile

    linkables_needed <- transitiveDeps <$> useWithSeparateFingerprintRule_ GetModuleGraphTransDepsFingerprints GetModuleGraph projectFile <*> pure (inputFilePath projectFile)
    linkables <- uses_ GetLinkable (projectFile : mapMaybe toProjectHaskellInput (maybe [] transitiveModuleDeps linkables_needed))
    -- We unset the global rdr env in mi_globals when we generate interfaces
    -- See Note [Clearing mi_globals after generating an iface]
    -- However, the eval plugin (setContext specifically) requires the rdr_env
    -- for the current module - so get it from the Typechecked Module and add
    -- it back to the iface for the current module.
    tm <- tmrTypechecked <$> use_ TypeCheck projectFile
    let rdr_env = tcg_rdr_env tm
        addRdrEnv hmi
          | iface <- hm_iface hmi
          , ms_mod ms == mi_module iface
#if MIN_VERSION_ghc(9,13,0)
          = hmi { hm_iface = set_mi_top_env (IfaceTopEnv (sortAvails $ gresToAvailInfo $ globalRdrEnvElts $ globalRdrEnvLocal rdr_env) (mkIfaceImports $ tcg_import_decls tm)) iface}
#elif MIN_VERSION_ghc(9,11,0)
          = hmi { hm_iface = set_mi_top_env (Just $ IfaceTopEnv (forceGlobalRdrEnv (globalRdrEnvLocal rdr_env)) (mkIfaceImports $ tcg_import_decls tm)) iface}
#else
          = hmi { hm_iface = iface { mi_globals = Just $!
#if MIN_VERSION_ghc(9,8,0)
                    forceGlobalRdrEnv
#endif
                      rdr_env
                }}
#endif
          | otherwise = hmi
#if MIN_VERSION_ghc(9,13,0)
    linkable_hsc <- liftIO $ loadModulesHome (map (addRdrEnv . linkableHomeMod) linkables) deps_hsc
#else
    let linkable_hsc = loadModulesHome (map (addRdrEnv . linkableHomeMod) linkables) deps_hsc
#endif
    return (ms, linkable_hsc)
  -- Bit awkward we need to use evalGhcEnv here but setContext requires to run
  -- in the Ghc monad
  env2 <- liftIO $ evalGhcEnv env1 $ do
#if MIN_VERSION_ghc(9,13,0)
            setContext [Compat.IIModule (ms_mod ms)]
#else
            setContext [Compat.IIModule (moduleName (ms_mod ms))]
#endif
            let df = flip xopt_set    LangExt.ExtendedDefaultRules
                   . flip xopt_unset  LangExt.MonomorphismRestriction
                   . flip gopt_set    Opt_ImplicitImportQualified
                   . flip gopt_unset  Opt_DiagnosticsShowCaret
                   . disableOptimisation
                   . setBackend ghciBackend
                   $ (ms_hspp_opts ms) {
                        useColor = Never
                      , canUseColor = False }
            modifyDynFlags (const df)
            when needs_quickcheck $ void $ addPackages ["QuickCheck"]
            getSession
  return env2

-- | Convert the typechecker's import specs into the interface representation,
-- so the reconstructed iface for the current module records what it imports
-- (needed when re-adding the rdr env, see 'initialiseSessionForEval').
#if MIN_VERSION_ghc(9,13,0)
mkIfaceImports :: [ImportUserSpec] -> [IfaceImport]
mkIfaceImports = map go
  where
    go (ImpUserSpec decl ImpUserAll) = IfaceImport decl ImpIfaceAll
    go (ImpUserSpec decl (ImpUserExplicit avails parents)) = IfaceImport decl (ImpIfaceExplicit (DefinitelyDeterministicAvails avails) (nameSetElemsStable parents))
    go (ImpUserSpec decl (ImpUserEverythingBut ns)) = IfaceImport decl (ImpIfaceEverythingBut (nameSetElemsStable ns))
#elif MIN_VERSION_ghc(9,11,0)
mkIfaceImports :: [ImportUserSpec] -> [IfaceImport]
mkIfaceImports = map go
  where
    go (ImpUserSpec decl ImpUserAll) = IfaceImport decl ImpIfaceAll
    go (ImpUserSpec decl (ImpUserExplicit env)) = IfaceImport decl (ImpIfaceExplicit (forceGlobalRdrEnv env))
    go (ImpUserSpec decl (ImpUserEverythingBut ns)) = IfaceImport decl (ImpIfaceEverythingBut ns)
#endif

-- | Prepend an edit adding a trailing newline when the module does not end in
-- one, so the appended results land on their own line.
addFinalReturn :: Text -> [TextEdit] -> [TextEdit]
addFinalReturn mdlText edits
    | not (null edits) && not (T.null mdlText) && T.last mdlText /= '\n' =
        finalReturn mdlText : edits
    | otherwise = edits

-- | An empty edit at the very end of the module that inserts a newline.
finalReturn :: Text -> TextEdit
finalReturn txt =
    let ls = T.lines txt
        l = fromIntegral $ length ls -1
        c = fromIntegral $ T.length $ maybe T.empty snd (unsnoc ls)
        p = Position l c
     in TextEdit (Range p p) "\n"

-- | The current (possibly unsaved) contents of the module as seen by the IDE.
moduleText :: IdeState -> Uri -> ExceptT PluginError (HandlerM config) Text
moduleText state uri = do
    contents <-
        handleMaybeM (PluginInternalError "mdlText") $
            liftIO $
                runAction "eval.getUriContents" state $
                    getUriContents $
                        toNormalizedUri uri
    pure $ Rope.toText contents

-- | Flatten sections into their individual 'EvalExpr's, tagging each with the index
-- ('EvalId') of its containing section.
evalExprsBySection :: [Section] -> [(Section, EvalId, EvalExpr)]
evalExprsBySection sections =
    [(section, ident, evalExpr)
    | (ident, section) <- zip [0..] sections
    , evalExpr <- sectionEvalExprs section
    ]

type TEnv = String
-- |GHC declarations required for expression evaluation
evalSetup :: Ghc ()
evalSetup = do
    preludeAsP  <- parseImportDecl "import qualified Prelude as P"
    -- 'myExecStmt' redirects the interpreted @stdout@ and @stderr@ to a temporary
    -- file in order to capture output produced as a side effect of evaluating a
    -- statement. The setup and teardown statements it injects need these modules
    -- in scope.
    systemIO    <- parseImportDecl "import qualified System.IO"
    ghcIOHandle <- parseImportDecl "import qualified GHC.IO.Handle"
    context     <- getContext
    setContext (IIDecl preludeAsP : IIDecl systemIO : IIDecl ghcIOHandle : context)

-- | Evaluate every 'EvalExpr' and produce the 'TextEdit's that write the results
-- back into the document, prefixing/padding each result line as the section's
-- format requires.
runEvalExprs ::
     Recorder (WithPriority Log)
  -> EvalConfig
  -> TEnv
  -> [(Section, EvalExpr)]
  -> Ghc [TextEdit]
runEvalExprs recorder EvalConfig{..} e evalExprs = do
    df <- getInteractiveDynFlags
    evalSetup
    when (hasQuickCheck df && needsQuickCheck evalExprs) $
      void $ evals recorder True e df propSetup

    mapM (processEvalExpr e df) evalExprs
  where
    processEvalExpr :: TEnv -> DynFlags -> (Section, EvalExpr) -> Ghc TextEdit
    processEvalExpr fp df (section, evalExpr) = do
        let dbg = logWith recorder Debug
            pre =
              (if isLiterate fp then ("> " `T.append`) else id) $
                padPrefix (sectionFormat section)
            pad = T.append pre
        rs <- runEvalExpr e df evalExpr
        dbg $ LogRunEvalExprResults rs

        let resultLines = evalExprCheck eval_cfg_diff (section, evalExpr) rs

        let edit = asEdit (sectionFormat section) evalExpr (map pad resultLines)
        dbg $ LogRunEvalExprEdits edit
        return edit

    runEvalExpr :: String -> DynFlags -> EvalExpr -> Ghc [Text]
    runEvalExpr e df evalExpr
        | not (hasQuickCheck df) && isProperty evalExpr =
            return $
                singleLine
                    "Add QuickCheck to your cabal dependencies to run this property."
        | otherwise =
            evals
              recorder
              (eval_cfg_exception && not (isProperty evalExpr))
              e
              df
              (asStatements evalExpr)

-- | Build the edit that replaces the old result of an 'EvalExpr' with
-- @resultLines@. For an 'EvalExpr' that sits on the closing @-}@ line of a
-- block comment, the result is inserted before @-}@ on fresh lines; otherwise
-- it simply overwrites the existing result range.
asEdit :: Format -> EvalExpr -> [Text] -> TextEdit
asEdit (MultiLine commRange) evalExpr resultLines
    -- An 'EvalExpr' in a block comment, ending with @-\}@ without newline in-between.
    | evalExprRange evalExpr ^. L.end . L.line == commRange ^. L.end . L.line
    =
    TextEdit
        (Range
            (evalExprRange evalExpr ^. L.end)
            (resultRange evalExpr ^. L.end)
        )
        ("\n" <> T.unlines (resultLines <> ["-}"]))
asEdit _ evalExpr resultLines =
    TextEdit (resultRange evalExpr) (T.unlines resultLines)

{- |
The result of evaluating an eval-expr line can be:
* a value
* nothing
* a (possibly multiline) error message

A value is returned for a correct expression.

Either a pure value:
>>> 'h' : "askell"
"haskell"

Or an 'IO a' (output on stdout/stderr is captured):
>>> putStrLn "Hello," >> pure "World!"
Hello,
"World!"

Note the quotes around @World!@, which are a result of using 'show'.

Nothing is returned for a correct directive:

>>>:set -XFlexibleInstances
>>> import Data.Maybe

Nothing is returned for a correct declaration (let..,x=, data, class)

>>> let x = 11
>>> y = 22
>>> data B = T | F
>>> class C a

Nothing is returned for an empty line:

>>>

A, possibly multi line, error is returned for a wrong declaration, directive or value or an exception thrown by the evaluated code:

>>>:set -XNonExistent
Some flags have not been recognized: -XNonExistent

>>> cls C
Illegal term-level use of the class `C'
  defined at <interactive>:1:2
In the first argument of `cls', namely `C'
In the expression: cls C
In an equation for `it_a1kSJ': it_a1kSJ = cls C
Variable not in scope: cls :: t0_a1kU9[tau:1] -> t1_a1kUb[tau:1]

>>> "A
lexical error at end of input

Exceptions are shown as if printed, but it can be configured to include prefix like
in GHCi or doctest. This allows it to be used as a hack to simulate print until we
get proper IO support. See #1977

>>> 3 `div` 0
divide by zero

>>> error "Something went wrong\nbad times" :: E.SomeException
Something went wrong
bad times

Or for a value that does not have a Show instance and can therefore not be displayed:
>>> data V = V
>>> V
No instance for `Show V' arising from a use of `evalPrint'
In a stmt of an interactive GHCi command: evalPrint it_a1l4V
-}
evals :: Recorder (WithPriority Log) -> Bool -> TEnv -> DynFlags -> [Statement] -> Ghc [Text]
evals recorder mark_exception fp df stmts = do
    er <- gStrictTry $ mapM eval stmts
    return $ case er of
        Left err -> errorLines err
        Right rs -> concat . catMaybes $ rs
  where
    dbg  = logWith recorder Debug
    eval :: Statement -> Ghc (Maybe [Text])
    eval (Located l stmt)
        | -- GHCi flags
          Just (words -> flags) <- parseSetFlags stmt = do
            dbg $ LogEvalFlags flags
            ndf <- getInteractiveDynFlags
            dbg $ LogEvalPreSetDynFlags ndf
#if MIN_VERSION_ghc(9,13,0)
            hsc_env <- getSession
            eans <-
                liftIO $ try @GhcException $
                parseDynamicFlagsCmdLine (hsc_logger hsc_env) ndf
                    (map (L $ UnhelpfulSpan unhelpfulReason) flags)
#else
            eans <-
                liftIO $ try @GhcException $
                parseDynamicFlagsCmdLine ndf
                (map (L $ UnhelpfulSpan unhelpfulReason) flags)
#endif
            dbg $ LogEvalParsedFlags eans
            case eans of
                Left err -> pure $ Just $ errorLines $ show err
                Right (df', ignoreds, warns) -> do
                    let warnings = do
                            guard $ not $ null warns
                            pure $ errorLines $
                                prettyWarnings warns
                        igns = do
                            guard $ not $ null ignoreds
                            pure
                                ["Some flags have not been recognized: "
                                <> T.pack (intercalate ", " $ map SrcLoc.unLoc ignoreds)
                                ]
                    dbg $ LogEvalPostSetDynFlags df'
                    setSessionAndInteractiveDynFlags df'
                    pure $ warnings <> igns
        | -- A type/kind command
          Just (cmd, arg) <- parseGhciLikeCmd $ T.pack stmt =
            evalGhciLikeCmd cmd arg
        | -- A statement
          isStmt pf stmt =
            do
                dbg $ LogEvalStmtStart stmt
                res <- exec stmt l
                let r = case res of
                        Left err -> Just . (if mark_exception then exceptionLines else errorLines) $ err
                        Right x  -> singleLine <$> x
                dbg $ LogEvalStmtResult r
                return r
        | -- An import
          isImport pf stmt =
            do
                dbg $ LogEvalImport stmt
                _ <- addImport stmt
                return Nothing
        | -- A declaration
          otherwise =
            do
                dbg $ LogEvalDeclaration stmt
                void $ runDecls stmt
                return Nothing
    pf = initParserOpts df
    unhelpfulReason = UnhelpfulInteractive
    exec stmt l =
        let opts = execOptions{execSourceFile = fp, execLineNumber = l}
         in execStmtCaptureResult recorder stmt opts

needsQuickCheck :: [(Section, EvalExpr)] -> Bool
needsQuickCheck = any (isProperty . snd)

hasQuickCheck :: DynFlags -> Bool
hasQuickCheck df = any (hasPackage df) ["QuickCheck", "QckChck"]

singleLine :: String -> [Text]
singleLine s = [T.pack s]

{- |
 Convert error messages to a list of text lines
 Remove unnecessary information.
-}
errorLines :: String -> [Text]
errorLines =
        dropWhileEnd T.null
        . takeWhile (not . (\x -> "CallStack" `T.isPrefixOf` x || "HasCallStack" `T.isPrefixOf` x))
        . T.lines
        . T.pack

{- |
 Convert exception messages to a list of text lines
 Remove unnecessary information and mark it as exception.
 We use '*** Exception:' to make it identical to doctest
 output, see #2353.
-}
exceptionLines :: String -> [Text]
exceptionLines = (ix 0 %~ ("*** Exception: " <>)) . errorLines

{- |
>>> map (pad_ (T.pack "--")) (map T.pack ["2+2",""])
["--2+2","--<BLANKLINE>"]
-}

padPrefix :: IsString p => Format -> p
padPrefix SingleLine = "-- "
padPrefix _          = ""

{- | Resulting @Text@ MUST NOT prefix each line with @--@
   Such comment-related post-process will be taken place
   solely in 'evalGhciLikeCmd'.
-}
type GHCiLikeCmd = DynFlags -> Text -> Ghc (Maybe Text)

-- Should we use some sort of trie here?
ghciLikeCommands :: [(Text, GHCiLikeCmd)]
ghciLikeCommands =
    [ ("info", doInfoCmd False)
    , ("info!", doInfoCmd True)
    , ("kind", doKindCmd False)
    , ("kind!", doKindCmd True)
    , ("type", doTypeCmd)
    ]

-- | Dispatch a GHCi-like command (e.g. @:type@, @:kind@, @:info@) to its
-- handler, matching by exact name or unique prefix. Throws if no command
-- matches.
evalGhciLikeCmd :: Text -> Text -> Ghc (Maybe [Text])
evalGhciLikeCmd cmd arg = do
    df <- getSessionDynFlags
    case lookup cmd ghciLikeCommands
        <|> snd
        <$> find (T.isPrefixOf cmd . fst) ghciLikeCommands of
        Just hndler ->
            fmap
                T.lines
                <$> hndler df arg
        _ -> E.throw $ GhciLikeCmdNotImplemented cmd arg

-- | Implement @:info@ / @:info!@: show the definition, fixity and instances of
-- each named thing. The 'Bool' is the @!@ variant, including all instances.
doInfoCmd :: Bool -> DynFlags -> Text -> Ghc (Maybe Text)
doInfoCmd allInfo dflags s = do
    sdocs <- mapM infoThing (T.words s)
    pure $ Just $ T.pack $ showSDoc dflags (vcat sdocs)
    where
        infoThing :: GHC.GhcMonad m => Text -> m SDoc
        infoThing (T.unpack -> str) = do
            names     <- GHC.parseName str
            mb_stuffs <- mapM (GHC.getInfo allInfo) names
            let filtered = filterOutChildren (\(t,_f,_ci,_fi,_sd) -> t)
                                            (catMaybes $ toList mb_stuffs)
            return $ vcat (intersperse (text "") $ map pprInfo filtered)

        filterOutChildren :: (a -> TyThing) -> [a] -> [a]
        filterOutChildren get_thing xs
            = filter (not . has_parent) xs
            where
                all_names = mkNameSet (map (getName . get_thing) xs)
                has_parent x = case tyThingParent_maybe (get_thing x) of
                                Just p  -> getName p `elemNameSet` all_names
                                Nothing -> False

        pprInfo :: (TyThing, Fixity, [GHC.ClsInst], [GHC.FamInst], SDoc) -> SDoc
        pprInfo (thing, fixity, cls_insts, fam_insts, docs)
            =  docs
            $$ pprTyThingInContextLoc thing
            $$ showFixity thing fixity
            $$ vcat (map GHC.pprInstance cls_insts)
            $$ vcat (map GHC.pprFamInst  fam_insts)

        pprTyThingInContextLoc :: TyThing -> SDoc
        pprTyThingInContextLoc tyThing
            = showWithLoc (pprDefinedAt (getName tyThing))
                          (pprTyThingInContext showToHeader tyThing)

        showWithLoc :: SDoc -> SDoc -> SDoc
        showWithLoc loc doc
            = hang doc 2 (text "\t--" <+> loc)

        showFixity :: TyThing -> Fixity -> SDoc
        showFixity thing fixity
            | fixity /= GHC.defaultFixity || isSymOcc (getOccName thing)
                = ppr fixity <+> pprInfixName (GHC.getName thing)
            | otherwise = empty

-- | Implement @:kind@ / @:kind!@: show a type's kind. The 'Bool' is the @!@
-- variant, additionally normalising and showing the type itself.
doKindCmd :: Bool -> DynFlags -> Text -> Ghc (Maybe Text)
doKindCmd False df arg = do
    let input = T.strip arg
    (_, kind) <- typeKind False $ T.unpack input
    let kindText = text (T.unpack input) <+> "::" <+> pprSigmaType kind
    pure $ Just $ T.pack (showSDoc df kindText)
doKindCmd True df arg = do
    let input = T.strip arg
    (ty, kind) <- typeKind True $ T.unpack input
    let kindDoc = text (T.unpack input) <+> "::" <+> pprSigmaType kind
        tyDoc = "=" <+> pprSigmaType ty
    pure $ Just $ T.pack (showSDoc df $ kindDoc $$ tyDoc)

-- | Implement @:type@: show the type of an expression. Accepts a leading
-- @+d@ to request the defaulted type (see 'parseExprMode').
doTypeCmd :: DynFlags -> Text -> Ghc (Maybe Text)
doTypeCmd dflags arg = do
    let (emod, expr) = parseExprMode arg
    ty <- GHC.exprType emod $ T.unpack expr
    let rawType = T.strip $ T.pack $ showSDoc dflags $ pprSigmaType ty
        broken = T.any (\c -> c == '\r' || c == '\n') rawType
    pure $
        Just $
            if broken
                then
                    T.pack $
                        showSDoc dflags $
                            text (T.unpack expr)
                                $$ nest 2 ("::" <+> pprSigmaType ty)
                else expr <> " :: " <> rawType <> "\n"

-- | Split a @:type@ argument into its mode and expression: a leading @+d@
-- selects defaulting ('TM_Default'), anything else the plain type ('TM_Inst').
parseExprMode :: Text -> (TcRnExprMode, T.Text)
parseExprMode rawArg = case T.break isSpace rawArg of
    ("+d", rest) -> (TM_Default, T.strip rest)
    _            -> (TM_Inst, rawArg)

data GhciLikeCmdException = GhciLikeCmdNotImplemented
    { ghciCmdName :: Text
    , ghciCmdArg  :: Text
    }

instance Show GhciLikeCmdException where
    showsPrec _ GhciLikeCmdNotImplemented{..} =
        showString "unknown command '"
            . showString (T.unpack ghciCmdName)
            . showChar '\''

instance E.Exception GhciLikeCmdException

{-
>>> parseGhciLikeCmd (T.pack ":kind! N + M + 1")
Just ("kind!","N + M + 1")
>>> parseGhciLikeCmd (T.pack ":kind a")
Just ("kind","a")
-}
parseGhciLikeCmd :: Text -> Maybe (Text, Text)
parseGhciLikeCmd input = do
    (':', rest) <- T.uncons $ T.stripStart input
    pure $ second T.strip $ T.break isSpace rest
