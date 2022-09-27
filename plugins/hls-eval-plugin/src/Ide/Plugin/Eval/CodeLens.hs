{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{- |
A plugin inspired by the REPLoid feature of <https://github.com/jyp/dante Dante>, <https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810775744 Haddock>'s Examples and Properties and <https://hackage.haskell.org/package/doctest Doctest>.

For a full example see the "Ide.Plugin.Eval.Tutorial" module.
-}
module Ide.Plugin.Eval.CodeLens (
    codeLens,
    evalCommand,
) where

import           Control.Applicative                          (Alternative ((<|>)))
import           Control.Arrow                                (second, (>>>))
import           Control.Exception                            (try)
import qualified Control.Exception                            as E
import           Control.Lens                                 (_1, _3, ix, (%~),
                                                               (<&>), (^.))
import           Control.Monad                                (guard, join,
                                                               void, when)
import           Control.Monad.IO.Class                       (MonadIO (liftIO))
import           Control.Monad.Trans.Except                   (ExceptT (..))
import           Data.Aeson                                   (toJSON)
import           Data.Char                                    (isSpace)
import           Data.Default
import qualified Data.HashMap.Strict                          as HashMap
import           Data.List                                    (dropWhileEnd,
                                                               find,
                                                               intercalate,
                                                               intersperse)
import           Data.Maybe                                   (catMaybes,
                                                               fromMaybe)
import           Data.String                                  (IsString)
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import           Data.Time                                    (getCurrentTime)
import           Data.Typeable                                (Typeable)
import           Development.IDE                              (GetModuleGraph (..),
                                                               GetLinkable (..),
                                                               GetModSummary (..),
                                                               GhcSessionIO (..),
                                                               IdeState,
                                                               ModSummaryResult (..),
                                                               NeedsCompilation (NeedsCompilation),
                                                               VFSModified (..),
                                                               evalGhcEnv,
                                                               hscEnvWithImportPaths,
                                                               linkableHomeMod,
                                                               printOutputable,
                                                               runAction,
                                                               textToStringBuffer,
                                                               toNormalizedFilePath',
                                                               uriToFilePath',
                                                               useNoFile_,
                                                               useWithStale_,
                                                               use_, uses_)
import           Development.IDE.Core.Rules                   (GhcSessionDepsConfig (..),
                                                               ghcSessionDepsDefinition)
import           Development.IDE.GHC.Compat                   hiding (typeKind,
                                                               unitState)
import qualified Development.IDE.GHC.Compat                   as Compat
import qualified Development.IDE.GHC.Compat                   as SrcLoc
import           Development.IDE.GHC.Compat.Util              (GhcException,
                                                               OverridingBool (..))
import           Development.IDE.Import.DependencyInformation (reachableModules)
import           Development.IDE.Types.Options
import           GHC                                          (ClsInst,
                                                               ExecOptions (execLineNumber, execSourceFile),
                                                               FamInst,
                                                               GhcMonad,
                                                               LoadHowMuch (LoadAllTargets),
                                                               NamedThing (getName),
                                                               defaultFixity,
                                                               execOptions,
                                                               exprType,
                                                               getInfo,
                                                               getInteractiveDynFlags,
                                                               isImport, isStmt,
                                                               load, parseName,
                                                               pprFamInst,
                                                               pprInstance,
                                                               setTargets,
                                                               typeKind)
#if MIN_VERSION_ghc(9,2,0)
import           GHC                                          (Fixity)
#endif
import qualified GHC.LanguageExtensions.Type                  as LangExt (Extension (..))

import           Development.IDE.Core.FileStore               (setSomethingModified)
import           Development.IDE.Types.Shake                  (toKey)
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Types.SrcLoc                             (UnhelpfulSpanReason (UnhelpfulInteractive))
#endif
import           Ide.Plugin.Eval.Code                         (Statement,
                                                               asStatements,
                                                               evalSetup,
                                                               myExecStmt,
                                                               propSetup,
                                                               resultRange,
                                                               testCheck,
                                                               testRanges)
import           Ide.Plugin.Eval.Config                       (EvalConfig (..),
                                                               getEvalConfig)
import           Ide.Plugin.Eval.GHC                          (addImport,
                                                               addPackages,
                                                               hasPackage,
                                                               showDynFlags)
import           Ide.Plugin.Eval.Parse.Comments               (commentsToSections)
import           Ide.Plugin.Eval.Parse.Option                 (parseSetFlags)
import           Ide.Plugin.Eval.Rules                        (queueForEvaluation)
import           Ide.Plugin.Eval.Types
import           Ide.Plugin.Eval.Util                         (gStrictTry,
                                                               isLiterate,
                                                               logWith,
                                                               response', timed)
import           Ide.PluginUtils                              (handleMaybe,
                                                               handleMaybeM,
                                                               pluginResponse)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types                           hiding
                                                              (SemanticTokenAbsolute (length, line),
                                                               SemanticTokenRelative (length))
import           Language.LSP.Types.Lens                      (end, line)
import           Language.LSP.VFS                             (virtualFileText)

#if MIN_VERSION_ghc(9,2,0)
#elif MIN_VERSION_ghc(9,0,0)
import           GHC.Driver.Session                           (unitDatabases,
                                                               unitState)
import           GHC.Types.SrcLoc                             (UnhelpfulSpanReason (UnhelpfulInteractive))
#else
import           DynFlags
#endif


{- | Code Lens provider
 NOTE: Invoked every time the document is modified, not just when the document is saved.
-}
codeLens :: PluginMethodHandler IdeState TextDocumentCodeLens
codeLens st plId CodeLensParams{_textDocument} =
    let dbg = logWith st
        perf = timed dbg
     in perf "codeLens" $
            pluginResponse $ do
                let TextDocumentIdentifier uri = _textDocument
                fp <- handleMaybe "uri" $ uriToFilePath' uri
                let nfp = toNormalizedFilePath' fp
                    isLHS = isLiterate fp
                dbg "fp" fp
                (comments, _) <- liftIO $
                    runAction "eval.GetParsedModuleWithComments" st $ useWithStale_ GetEvalComments nfp
                -- dbg "excluded comments" $ show $  DL.toList $
                --     foldMap (\(L a b) ->
                --         case b of
                --             AnnLineComment{}  -> mempty
                --             AnnBlockComment{} -> mempty
                --             _                 -> DL.singleton (a, b)
                --     )
                --     $ apiAnnComments' pm_annotations
                dbg "comments" $ show comments

                -- Extract tests from source code
                let Sections{..} = commentsToSections isLHS comments
                    tests = testsBySection nonSetupSections
                    cmd = mkLspCommand plId evalCommandName "Evaluate=..." (Just [])
                let lenses =
                        [ CodeLens testRange (Just cmd') Nothing
                        | (section, ident, test) <- tests
                        , let (testRange, resultRange) = testRanges test
                              args = EvalParams (setupSections ++ [section]) _textDocument ident
                              cmd' =
                                (cmd :: Command)
                                    { _arguments = Just (List [toJSON args])
                                    , _title =
                                        if trivial resultRange
                                            then "Evaluate..."
                                            else "Refresh..."
                                    }
                        ]

                perf "tests" $
                    dbg "Tests" $
                        unwords
                            [ show (length tests)
                            , "tests in"
                            , show (length nonSetupSections)
                            , "sections"
                            , show (length setupSections)
                            , "setups"
                            , show (length lenses)
                            , "lenses."
                            ]

                return $ List lenses
  where
    trivial (Range p p') = p == p'

evalCommandName :: CommandId
evalCommandName = "evalCommand"

evalCommand :: PluginId -> PluginCommand IdeState
evalCommand plId = PluginCommand evalCommandName "evaluate" (runEvalCmd plId)

type EvalId = Int

runEvalCmd :: PluginId -> CommandFunction IdeState EvalParams
runEvalCmd plId st EvalParams{..} =
    let dbg = logWith st
        perf = timed dbg
        cmd :: ExceptT String (LspM Config) WorkspaceEdit
        cmd = do
            let tests = map (\(a,_,b) -> (a,b)) $ testsBySection sections

            let TextDocumentIdentifier{_uri} = module_
            fp <- handleMaybe "uri" $ uriToFilePath' _uri
            let nfp = toNormalizedFilePath' fp
            mdlText <- moduleText _uri

            -- enable codegen
            liftIO $ queueForEvaluation st nfp
            liftIO $ setSomethingModified VFSUnmodified st [toKey NeedsCompilation nfp] "Eval"

            session <- runGetSession st nfp

            ms <- fmap msrModSummary $
                liftIO $
                    runAction "runEvalCmd.getModSummary" st $
                        use_ GetModSummary nfp

            now <- liftIO getCurrentTime

            let modName = moduleName $ ms_mod ms
                thisModuleTarget =
                    Target
                        (TargetFile fp Nothing)
                        False
                        (Just (textToStringBuffer mdlText, now))

            -- Setup environment for evaluation
            hscEnv' <- ExceptT $ fmap join $ liftIO . gStrictTry . evalGhcEnv session $ do
                env <- getSession

                -- Install the module pragmas and options
                df <- liftIO $ setupDynFlagsForGHCiLike env $ ms_hspp_opts ms

                -- Restore the original import paths
                let impPaths = importPaths $ hsc_dflags env
                df <- return df{importPaths = impPaths}

                -- Set the modified flags in the session
                _lp <- setSessionDynFlags df

                -- property tests need QuickCheck
                when (needsQuickCheck tests) $ void $ addPackages ["QuickCheck"]
                dbg "QUICKCHECK NEEDS" $ needsQuickCheck tests
                dbg "QUICKCHECK HAS" $ hasQuickCheck df

                -- copy the package state to the interactive DynFlags
                idflags <- getInteractiveDynFlags
                df <- getSessionDynFlags
                -- set the identical DynFlags as GHCi
                -- Source: https://github.com/ghc/ghc/blob/5abf59976c7335df760e5d8609d9488489478173/ghc/GHCi/UI.hs#L473-L483
                -- This needs to be done manually since the default flags are not visible externally.
                let df' = flip xopt_set    LangExt.ExtendedDefaultRules
                        . flip xopt_unset  LangExt.MonomorphismRestriction
                        $ idflags
                setInteractiveDynFlags $ df'
#if MIN_VERSION_ghc(9,0,0)
                        {
                        packageFlags =
                            packageFlags
                                df
                        , useColor = Never
                        , canUseColor = False
                        }
#else
                        { pkgState =
                            pkgState
                                df
                        , pkgDatabase =
                            pkgDatabase
                                df
                        , packageFlags =
                            packageFlags
                                df
                        , useColor = Never
                        , canUseColor = False
                        }
#endif

                -- Load the module with its current content (as the saved module might not be up to date)
                eSetTarget <- gStrictTry $ setTargets [thisModuleTarget]
                dbg "setTarget" eSetTarget

                -- load the module in the interactive environment
                loadResult <- perf "loadModule" $ load LoadAllTargets
                dbg "LOAD RESULT" $ printOutputable loadResult
                case loadResult of
                    Failed -> liftIO $ do
                        let err = ""
                        dbg "load ERR" err
                        return $ Left err
                    Succeeded -> do
                        -- Evaluation takes place 'inside' the module
                        setContext [Compat.IIModule modName]
                        Right <$> getSession
            evalCfg <- liftIO $ runAction "eval: config" st $ getEvalConfig plId

            -- Get linkables for all modules below us
            -- This can be optimised to only get the linkables for the symbols depended on by
            -- the statement we are parsing
            lbs <- liftIO $ runAction "eval: GetLinkables" st $ do
              linkables_needed <- reachableModules <$> useNoFile_ GetModuleGraph
              uses_ GetLinkable (filter (/= nfp) linkables_needed) -- We don't need the linkable for the current module
            let hscEnv'' = hscEnv' { hsc_HPT  = addListToHpt (hsc_HPT hscEnv') [(moduleName $ mi_module $ hm_iface hm, hm) | lb <- lbs, let hm = linkableHomeMod lb] }

            edits <-
                perf "edits" $
                    liftIO $
                        evalGhcEnv hscEnv'' $
                            runTests
                                evalCfg
                                (st, fp)
                                tests

            let workspaceEditsMap = HashMap.fromList [(_uri, List $ addFinalReturn mdlText edits)]
            let workspaceEdits = WorkspaceEdit (Just workspaceEditsMap) Nothing Nothing

            return workspaceEdits
     in perf "evalCmd" $
            withIndefiniteProgress "Evaluating" Cancellable $
                response' cmd

addFinalReturn :: Text -> [TextEdit] -> [TextEdit]
addFinalReturn mdlText edits
    | not (null edits) && not (T.null mdlText) && T.last mdlText /= '\n' =
        finalReturn mdlText : edits
    | otherwise = edits

finalReturn :: Text -> TextEdit
finalReturn txt =
    let ls = T.lines txt
        l = fromIntegral $ length ls -1
        c = fromIntegral $ T.length . last $ ls
        p = Position l c
     in TextEdit (Range p p) "\n"

moduleText :: (IsString e, MonadLsp c m) => Uri -> ExceptT e m Text
moduleText uri =
    handleMaybeM "mdlText" $
      (virtualFileText <$>)
          <$> getVirtualFile
              (toNormalizedUri uri)

testsBySection :: [Section] -> [(Section, EvalId, Test)]
testsBySection sections =
    [(section, ident, test)
    | (ident, section) <- zip [0..] sections
    , test <- sectionTests section
    ]

type TEnv = (IdeState, String)

runTests :: EvalConfig -> TEnv -> [(Section, Test)] -> Ghc [TextEdit]
runTests EvalConfig{..} e@(_st, _) tests = do
    df <- getInteractiveDynFlags
    evalSetup
    when (hasQuickCheck df && needsQuickCheck tests) $ void $ evals True e df propSetup

    mapM (processTest e df) tests
  where
    processTest :: TEnv -> DynFlags -> (Section, Test) -> Ghc TextEdit
    processTest e@(st, fp) df (section, test) = do
        let dbg = logWith st
        let pad = pad_ $ (if isLiterate fp then ("> " `T.append`) else id) $ padPrefix (sectionFormat section)

        rs <- runTest e df test
        dbg "TEST RESULTS" rs

        let checkedResult = testCheck eval_cfg_diff (section, test) rs
        let resultLines = concatMap T.lines checkedResult

        let edit = asEdit (sectionFormat section) test (map pad resultLines)
        dbg "TEST EDIT" edit
        return edit

    -- runTest :: String -> DynFlags -> Loc Test -> Ghc [Text]
    runTest _ df test
        | not (hasQuickCheck df) && isProperty test =
            return $
                singleLine
                    "Add QuickCheck to your cabal dependencies to run this test."
    runTest e df test = evals (eval_cfg_exception && not (isProperty test)) e df (asStatements test)

asEdit :: Format -> Test -> [Text] -> TextEdit
asEdit (MultiLine commRange) test resultLines
    -- A test in a block comment, ending with @-\}@ without newline in-between.
    | testRange test ^. end.line == commRange ^. end . line
    =
    TextEdit
        (Range
            (testRange test ^. end)
            (resultRange test ^. end)
        )
        ("\n" <> T.unlines (resultLines <> ["-}"]))
asEdit _ test resultLines =
    TextEdit (resultRange test) (T.unlines resultLines)

{-
The result of evaluating a test line can be:
* a value
* nothing
* a (possibly multiline) error message

A value is returned for a correct expression.

Either a pure value:
>>> 'h' :"askell"
"haskell"

Or an 'IO a' (output on stdout/stderr is ignored):
>>> print "OK" >> return "ABC"
"ABC"

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
Variable not in scope: cls :: t0 -> t
Data constructor not in scope: C

>>> "A
lexical error in string/character literal at end of input

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
No instance for (Show V) arising from a use of ‘evalPrint’
-}
evals :: Bool -> TEnv -> DynFlags -> [Statement] -> Ghc [Text]
evals mark_exception (st, fp) df stmts = do
    er <- gStrictTry $ mapM eval stmts
    return $ case er of
        Left err -> errorLines err
        Right rs -> concat . catMaybes $ rs
  where
    dbg = logWith st
    eval :: Statement -> Ghc (Maybe [Text])
    eval (Located l stmt)
        | -- GHCi flags
          Just (words -> flags) <- parseSetFlags stmt = do
            dbg "{:SET" flags
            ndf <- getInteractiveDynFlags
            dbg "pre set" $ showDynFlags ndf
            eans <-
                liftIO $ try @GhcException $
                parseDynamicFlagsCmdLine ndf
                (map (L $ UnhelpfulSpan unhelpfulReason) flags)
            dbg "parsed flags" $ eans
              <&> (_1 %~ showDynFlags >>> _3 %~ map warnMsg)
            case eans of
                Left err -> pure $ Just $ errorLines $ show err
                Right (df', ignoreds, warns) -> do
                    let warnings = do
                            guard $ not $ null warns
                            pure $ errorLines $
                                unlines $
                                map prettyWarn warns
                        igns = do
                            guard $ not $ null ignoreds
                            pure
                                ["Some flags have not been recognized: "
                                <> T.pack (intercalate ", " $ map SrcLoc.unLoc ignoreds)
                                ]
                    dbg "post set" $ showDynFlags df'
                    _ <- setSessionDynFlags df'
                    sessDyns <- getSessionDynFlags
                    setInteractiveDynFlags sessDyns
                    pure $ warnings <> igns
        | -- A type/kind command
          Just (cmd, arg) <- parseGhciLikeCmd $ T.pack stmt =
            evalGhciLikeCmd cmd arg
        | -- A statement
          isStmt pf stmt =
            do
                dbg "{STMT " stmt
                res <- exec stmt l
                let r = case res of
                        Left err -> Just . (if mark_exception then exceptionLines else errorLines) $ err
                        Right x  -> singleLine <$> x
                dbg "STMT} -> " r
                return r
        | -- An import
          isImport pf stmt =
            do
                dbg "{IMPORT " stmt
                _ <- addImport stmt
                return Nothing
        | -- A declaration
          otherwise =
            do
                dbg "{DECL " stmt
                void $ runDecls stmt
                return Nothing
    pf = initParserOpts df
#if !MIN_VERSION_ghc(9,0,0)
    unhelpfulReason = "<interactive>"
#else
    unhelpfulReason = UnhelpfulInteractive
#endif
    exec stmt l =
        let opts = execOptions{execSourceFile = fp, execLineNumber = l}
         in myExecStmt stmt opts

prettyWarn :: Warn -> String
prettyWarn Warn{..} =
    T.unpack (printOutputable $ SrcLoc.getLoc warnMsg) <> ": warning:\n"
    <> "    " <> SrcLoc.unLoc warnMsg

runGetSession :: MonadIO m => IdeState -> NormalizedFilePath -> m HscEnv
runGetSession st nfp = liftIO $ runAction "eval" st $ do
    -- Create a new GHC Session rather than reusing an existing one
    -- to avoid interfering with ghcide
    -- UPDATE: I suspect that this doesn't really work, we always get the same Session
    --         we probably cache hscEnvs in the Session state
    IdeGhcSession{loadSessionFun} <- useNoFile_ GhcSessionIO
    let fp = fromNormalizedFilePath nfp
    ((_, res),_) <- liftIO $ loadSessionFun fp
    let env = fromMaybe (error $ "Unknown file: " <> fp) res
        ghcSessionDepsConfig = def
            { checkForImportCycles = False
            }
    res <- fmap hscEnvWithImportPaths <$> ghcSessionDepsDefinition True ghcSessionDepsConfig env nfp
    return $ fromMaybe (error $ "Unable to load file: " <> fp) res

needsQuickCheck :: [(Section, Test)] -> Bool
needsQuickCheck = any (isProperty . snd)

hasQuickCheck :: DynFlags -> Bool
hasQuickCheck df = hasPackage df "QuickCheck"

singleLine :: String -> [Text]
singleLine s = [T.pack s]

{- |
 Convert error messages to a list of text lines
 Remove unnecessary information.
-}
errorLines :: String -> [Text]
errorLines =
        dropWhileEnd T.null
        . takeWhile (not . ("CallStack" `T.isPrefixOf`))
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
pad_ :: Text -> Text -> Text
pad_ prefix = (prefix `T.append`) . convertBlank

convertBlank :: Text -> Text
convertBlank x
    | T.null x = "<BLANKLINE>"
    | otherwise = x

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
                                            (catMaybes mb_stuffs)
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

doKindCmd :: Bool -> DynFlags -> Text -> Ghc (Maybe Text)
doKindCmd False df arg = do
    let input = T.strip arg
    (_, kind) <- typeKind False $ T.unpack input
    let kindText = text (T.unpack input) <+> "::" <+> pprTypeForUser kind
    pure $ Just $ T.pack (showSDoc df kindText)
doKindCmd True df arg = do
    let input = T.strip arg
    (ty, kind) <- typeKind True $ T.unpack input
    let kindDoc = text (T.unpack input) <+> "::" <+> pprTypeForUser kind
        tyDoc = "=" <+> pprTypeForUser ty
    pure $ Just $ T.pack (showSDoc df $ kindDoc $$ tyDoc)

doTypeCmd :: DynFlags -> Text -> Ghc (Maybe Text)
doTypeCmd dflags arg = do
    let (emod, expr) = parseExprMode arg
    ty <- GHC.exprType emod $ T.unpack expr
    let rawType = T.strip $ T.pack $ showSDoc dflags $ pprTypeForUser ty
        broken = T.any (\c -> c == '\r' || c == '\n') rawType
    pure $
        Just $
            if broken
                then
                    T.pack $
                        showSDoc dflags $
                            text (T.unpack expr)
                                $$ nest 2 ("::" <+> pprTypeForUser ty)
                else expr <> " :: " <> rawType <> "\n"

parseExprMode :: Text -> (TcRnExprMode, T.Text)
parseExprMode rawArg = case T.break isSpace rawArg of
#if !MIN_VERSION_ghc(9,2,0)
    ("+v", rest) -> (TM_NoInst, T.strip rest)
#endif
    ("+d", rest) -> (TM_Default, T.strip rest)
    _            -> (TM_Inst, rawArg)

data GhciLikeCmdException = GhciLikeCmdNotImplemented
    { ghciCmdName :: Text
    , ghciCmdArg  :: Text
    }
    deriving (Typeable)

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

setupDynFlagsForGHCiLike :: HscEnv -> DynFlags -> IO DynFlags
setupDynFlagsForGHCiLike env dflags = do
    let dflags3 = setInterpreterLinkerOptions dflags
        platform = targetPlatform dflags3
        evalWays = Compat.hostFullWays
        dflags3a = setWays evalWays dflags3
        dflags3b =
            foldl gopt_set dflags3a $
                concatMap (Compat.wayGeneralFlags platform) evalWays
        dflags3c =
            foldl gopt_unset dflags3b $
                concatMap (Compat.wayUnsetGeneralFlags platform) evalWays
        dflags4 =
            dflags3c
                `gopt_set` Opt_ImplicitImportQualified
                `gopt_set` Opt_IgnoreOptimChanges
                `gopt_set` Opt_IgnoreHpcChanges
                `gopt_unset` Opt_DiagnosticsShowCaret
    Compat.hsc_dflags <$> Compat.initializePlugins (Compat.hscSetFlags dflags4 env)
