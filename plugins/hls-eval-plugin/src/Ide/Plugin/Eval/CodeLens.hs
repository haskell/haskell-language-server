{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
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

import           CmdLineParser
import           Control.Applicative                  (Alternative ((<|>)))
import           Control.Arrow                        (second, (>>>))
import           Control.Exception                    (try)
import qualified Control.Exception                    as E
import           Control.Lens                         (_1, _3, (%~), (<&>),
                                                       (^.))
import           Control.Monad                        (guard, join, void, when)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Except           (ExceptT (..))
import           Data.Aeson                           (toJSON)
import           Data.Char                            (isSpace)
import qualified Data.DList                           as DL
import qualified Data.HashMap.Strict                  as HashMap
import           Data.List                            (dropWhileEnd, find,
                                                       intercalate)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (catMaybes, fromMaybe)
import           Data.String                          (IsString)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time                            (getCurrentTime)
import           Data.Typeable                        (Typeable)
import           Development.IDE                      (Action,
                                                       GetDependencies (..),
                                                       GetModIface (..),
                                                       GetModSummary (..),
                                                       GetParsedModuleWithComments (..),
                                                       GhcSessionIO (..),
                                                       HiFileResult (hirHomeMod, hirModSummary),
                                                       HscEnvEq, IdeState,
                                                       ModSummaryResult (..),
                                                       evalGhcEnv,
                                                       hscEnvWithImportPaths,
                                                       prettyPrint,
                                                       realSrcSpanToRange,
                                                       runAction,
                                                       textToStringBuffer,
                                                       toNormalizedFilePath',
                                                       uriToFilePath',
                                                       useNoFile_,
                                                       useWithStale_, use_,
                                                       uses_)
import           Development.IDE.Core.Compile         (loadModulesHome,
                                                       setupFinderCache)
import           Development.IDE.Core.PositionMapping (toCurrentRange)
import           Development.IDE.Core.Rules           (TransitiveDependencies (transitiveModuleDeps))
import           Development.IDE.GHC.Compat           (AnnotationComment (AnnBlockComment, AnnLineComment),
                                                       GenLocated (L),
                                                       GhcException, HscEnv,
                                                       ParsedModule (..),
                                                       SrcSpan (RealSrcSpan, UnhelpfulSpan),
                                                       setInteractiveDynFlags,
                                                       srcSpanFile)
import qualified Development.IDE.GHC.Compat           as SrcLoc
import           Development.IDE.Types.Options
import           DynamicLoading                       (initializePlugins)
import           FastString                           (unpackFS)
import           GHC                                  (ExecOptions (execLineNumber, execSourceFile),
                                                       GeneralFlag (..), Ghc,
                                                       GhcLink (LinkInMemory),
                                                       GhcMode (CompManager),
                                                       GhcMonad (getSession),
                                                       HscTarget (HscInterpreted),
                                                       LoadHowMuch (LoadAllTargets),
                                                       ModSummary (ms_hspp_opts),
                                                       Module (moduleName),
                                                       SuccessFlag (Failed, Succeeded),
                                                       TcRnExprMode (..),
                                                       execOptions, exprType,
                                                       getInteractiveDynFlags,
                                                       getSessionDynFlags,
                                                       isImport, isStmt, load,
                                                       runDecls, setContext,
                                                       setLogAction,
                                                       setSessionDynFlags,
                                                       setTargets, typeKind)
import           GhcPlugins                           (DynFlags (..),
                                                       defaultLogActionHPutStrDoc,
                                                       gopt_set, gopt_unset,
                                                       hsc_dflags, interpWays,
                                                       parseDynamicFlagsCmdLine,
                                                       targetPlatform,
                                                       updateWays,
                                                       wayGeneralFlags,
                                                       wayUnsetGeneralFlags,
                                                       xopt_set)
import           HscTypes                             (InteractiveImport (IIModule),
                                                       ModSummary (ms_mod),
                                                       Target (Target),
                                                       TargetId (TargetFile))
import           Ide.Plugin.Eval.Code                 (Statement, asStatements,
                                                       evalExtensions,
                                                       evalSetup, myExecStmt,
                                                       propSetup, resultRange,
                                                       testCheck, testRanges)
import           Ide.Plugin.Eval.GHC                  (addImport, addPackages,
                                                       hasPackage, showDynFlags)
import           Ide.Plugin.Eval.Parse.Comments       (commentsToSections)
import           Ide.Plugin.Eval.Parse.Option         (parseSetFlags)
import           Ide.Plugin.Eval.Types
import           Ide.Plugin.Eval.Util                 (asS, gStrictTry,
                                                       handleMaybe,
                                                       handleMaybeM, isLiterate,
                                                       logWith, response,
                                                       response', timed)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           Language.LSP.Types.Lens              (end, line)
import           Language.LSP.VFS                     (virtualFileText)
import           Outputable                           (nest, ppr, showSDoc,
                                                       text, ($$), (<+>))
import           System.FilePath                      (takeFileName)
import           System.IO                            (hClose)
import           UnliftIO.Temporary                   (withSystemTempFile)
import           Util                                 (OverridingBool (Never))

{- | Code Lens provider
 NOTE: Invoked every time the document is modified, not just when the document is saved.
-}
codeLens :: PluginMethodHandler IdeState TextDocumentCodeLens
codeLens st plId CodeLensParams{_textDocument} =
    let dbg = logWith st
        perf = timed dbg
     in perf "codeLens" $
            response $ do
                let TextDocumentIdentifier uri = _textDocument
                fp <- handleMaybe "uri" $ uriToFilePath' uri
                let nfp = toNormalizedFilePath' fp
                    isLHS = isLiterate fp
                dbg "fp" fp
                (ParsedModule{..}, posMap) <- liftIO $
                    runAction "parsed" st $ useWithStale_ GetParsedModuleWithComments nfp
                let comments = foldMap
                        ( foldMap $ \case
                            L (RealSrcSpan real) bdy
                                | unpackFS (srcSpanFile real) ==
                                    fromNormalizedFilePath nfp
                                , let ran0 = realSrcSpanToRange real
                                , Just curRan <- toCurrentRange posMap ran0
                                ->

                                    -- since Haddock parsing is unset explicitly in 'getParsedModuleWithComments',
                                    -- we can concentrate on these two
                                    case bdy of
                                        AnnLineComment cmt ->
                                            mempty { lineComments = Map.singleton curRan (RawLineComment cmt) }
                                        AnnBlockComment cmt ->
                                            mempty { blockComments = Map.singleton curRan $ RawBlockComment cmt }
                                        _ -> mempty
                            _ -> mempty
                        )
                        $ snd pm_annotations
                dbg "excluded comments" $ show $  DL.toList $
                    foldMap
                    (foldMap $ \(L a b) ->
                        case b of
                            AnnLineComment{}  -> mempty
                            AnnBlockComment{} -> mempty
                            _                 -> DL.singleton (a, b)
                    )
                    $ snd pm_annotations
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

evalCommand :: PluginCommand IdeState
evalCommand = PluginCommand evalCommandName "evaluate" runEvalCmd

type EvalId = Int

runEvalCmd :: CommandFunction IdeState EvalParams
runEvalCmd st EvalParams{..} =
    let dbg = logWith st
        perf = timed dbg
        cmd :: ExceptT String (LspM c) WorkspaceEdit
        cmd = do
            let tests = map (\(a,_,b) -> (a,b)) $ testsBySection sections

            let TextDocumentIdentifier{_uri} = module_
            fp <- handleMaybe "uri" $ uriToFilePath' _uri
            let nfp = toNormalizedFilePath' fp
            mdlText <- moduleText _uri

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
            hscEnv' <- ExceptT $ fmap join $ withSystemTempFile (takeFileName fp) $ \logFilename logHandle -> liftIO . gStrictTry . evalGhcEnv session $ do
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
                setInteractiveDynFlags $
                    (foldl xopt_set idflags evalExtensions)
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

                -- set up a custom log action
                setLogAction $ \_df _wr _sev _span _style _doc ->
                    defaultLogActionHPutStrDoc _df logHandle _doc _style

                -- Load the module with its current content (as the saved module might not be up to date)
                -- BUG: this fails for files that requires preprocessors (e.g. CPP) for ghc < 8.8
                -- see https://gitlab.haskell.org/ghc/ghc/-/issues/17066
                -- and https://hackage.haskell.org/package/ghc-8.10.1/docs/GHC.html#v:TargetFile
                eSetTarget <- gStrictTry $ setTargets [thisModuleTarget]
                dbg "setTarget" eSetTarget

                -- load the module in the interactive environment
                loadResult <- perf "loadModule" $ load LoadAllTargets
                dbg "LOAD RESULT" $ asS loadResult
                case loadResult of
                    Failed -> liftIO $ do
                        hClose logHandle
                        err <- readFile logFilename
                        dbg "load ERR" err
                        return $ Left err
                    Succeeded -> do
                        -- Evaluation takes place 'inside' the module
                        setContext [IIModule modName]
                        Right <$> getSession

            edits <-
                perf "edits" $
                    liftIO $
                        evalGhcEnv hscEnv' $
                            runTests
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
        l = length ls -1
        c = T.length . last $ ls
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

runTests :: TEnv -> [(Section, Test)] -> Ghc [TextEdit]
runTests e@(_st, _) tests = do
    df <- getInteractiveDynFlags
    evalSetup
    when (hasQuickCheck df && needsQuickCheck tests) $ void $ evals e df propSetup

    mapM (processTest e df) tests
  where
    processTest :: TEnv -> DynFlags -> (Section, Test) -> Ghc TextEdit
    processTest e@(st, fp) df (section, test) = do
        let dbg = logWith st
        let pad = pad_ $ (if isLiterate fp then ("> " `T.append`) else id) $ padPrefix (sectionFormat section)

        rs <- runTest e df test
        dbg "TEST RESULTS" rs

        let checkedResult = testCheck (section, test) rs

        let edit = asEdit (sectionFormat section) test (map pad checkedResult)
        dbg "TEST EDIT" edit
        return edit

    -- runTest :: String -> DynFlags -> Loc Test -> Ghc [Text]
    runTest _ df test
        | not (hasQuickCheck df) && isProperty test =
            return $
                singleLine
                    "Add QuickCheck to your cabal dependencies to run this test."
    runTest e df test = evals e df (asStatements test)

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
Unknown extension: "NonExistent"

>>> cls C
Variable not in scope: cls :: t0 -> ()
Data constructor not in scope: C

>>> "A
lexical error in string/character literal at end of input

>>> 3 `div` 0
divide by zero

>>> error "Something went wrong\nbad times" :: E.SomeException
Something went wrong
bad times

Or for a value that does not have a Show instance and can therefore not be displayed:
>>> data V = V
>>> V
No instance for (Show V)
-}
evals :: TEnv -> DynFlags -> [Statement] -> Ghc [Text]
evals (st, fp) df stmts = do
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
                (map (L $ UnhelpfulSpan "<interactive>") flags)
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
          isStmt df stmt =
            do
                dbg "{STMT " stmt
                res <- exec stmt l
                r <- case res of
                    Left err -> return . Just . errorLines $ err
                    Right x  -> return $ singleLine <$> x
                dbg "STMT} -> " r
                return r
        | -- An import
          isImport df stmt =
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
    exec stmt l =
        let opts = execOptions{execSourceFile = fp, execLineNumber = l}
         in myExecStmt stmt opts

prettyWarn :: Warn -> String
prettyWarn Warn{..} =
    prettyPrint (SrcLoc.getLoc warnMsg) <> ": warning:\n"
    <> "    " <> SrcLoc.unLoc warnMsg

ghcSessionDepsDefinition :: HscEnvEq -> NormalizedFilePath -> Action HscEnv
ghcSessionDepsDefinition env file = do
        let hsc = hscEnvWithImportPaths env
        deps <- use_ GetDependencies file
        let tdeps = transitiveModuleDeps deps
        ifaces <- uses_ GetModIface tdeps

        -- Currently GetDependencies returns things in topological order so A comes before B if A imports B.
        -- We need to reverse this as GHC gets very unhappy otherwise and complains about broken interfaces.
        -- Long-term we might just want to change the order returned by GetDependencies
        let inLoadOrder = reverse (map hirHomeMod ifaces)

        liftIO $ loadModulesHome inLoadOrder <$> setupFinderCache (map hirModSummary ifaces) hsc

runGetSession :: MonadIO m => IdeState -> NormalizedFilePath -> m HscEnv
runGetSession st nfp = liftIO $ runAction "eval" st $ do
    -- Create a new GHC Session rather than reusing an existing one
    -- to avoid interfering with ghcide
    IdeGhcSession{loadSessionFun} <- useNoFile_ GhcSessionIO
    let fp = fromNormalizedFilePath nfp
    ((_, res),_) <- liftIO $ loadSessionFun fp
    let hscEnv = fromMaybe (error $ "Unknown file: " <> fp) res
    ghcSessionDepsDefinition hscEnv nfp

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
    [("kind", doKindCmd False), ("kind!", doKindCmd True), ("type", doTypeCmd)]

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

doKindCmd :: Bool -> DynFlags -> Text -> Ghc (Maybe Text)
doKindCmd False df arg = do
    let input = T.strip arg
    (_, kind) <- typeKind False $ T.unpack input
    let kindText = text (T.unpack input) <+> "::" <+> ppr kind
    pure $ Just $ T.pack (showSDoc df kindText)
doKindCmd True df arg = do
    let input = T.strip arg
    (ty, kind) <- typeKind True $ T.unpack input
    let kindDoc = text (T.unpack input) <+> "::" <+> ppr kind
        tyDoc = "=" <+> ppr ty
    pure $ Just $ T.pack (showSDoc df $ kindDoc $$ tyDoc)

doTypeCmd :: DynFlags -> Text -> Ghc (Maybe Text)
doTypeCmd dflags arg = do
    let (emod, expr) = parseExprMode arg
    ty <- exprType emod $ T.unpack expr
    let rawType = T.strip $ T.pack $ showSDoc dflags $ ppr ty
        broken = T.any (\c -> c == '\r' || c == '\n') rawType
    pure $
        Just $
            if broken
                then
                    T.pack $
                        showSDoc dflags $
                            text (T.unpack expr)
                                $$ nest 2 ("::" <+> ppr ty)
                else expr <> " :: " <> rawType <> "\n"

parseExprMode :: Text -> (TcRnExprMode, T.Text)
parseExprMode rawArg = case T.break isSpace rawArg of
    ("+v", rest) -> (TM_NoInst, T.strip rest)
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
    let dflags3 =
            dflags
                { hscTarget = HscInterpreted
                , ghcMode = CompManager
                , ghcLink = LinkInMemory
                }
        platform = targetPlatform dflags3
        dflags3a = updateWays $ dflags3{ways = interpWays}
        dflags3b =
            foldl gopt_set dflags3a $
                concatMap (wayGeneralFlags platform) interpWays
        dflags3c =
            foldl gopt_unset dflags3b $
                concatMap (wayUnsetGeneralFlags platform) interpWays
        dflags4 =
            dflags3c
                `gopt_set` Opt_ImplicitImportQualified
                `gopt_set` Opt_IgnoreOptimChanges
                `gopt_set` Opt_IgnoreHpcChanges
                `gopt_unset` Opt_DiagnosticsShowCaret
    initializePlugins env dflags4
