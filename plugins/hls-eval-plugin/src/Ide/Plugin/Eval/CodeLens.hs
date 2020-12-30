{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{- |
A plugin inspired by the REPLoid feature of <https://github.com/jyp/dante Dante>, <https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810775744 Haddock>'s Examples and Properties and <https://hackage.haskell.org/package/doctest Doctest>.

For a full example see the "Ide.Plugin.Eval.Tutorial" module.
-}
module Ide.Plugin.Eval.CodeLens (
    codeLens,
    evalCommand,
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (second)
import qualified Control.Exception as E
import Control.Monad (
    void,
    when,
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (
    ExceptT (..),
    runExceptT,
 )
import Data.Aeson (
    FromJSON,
    ToJSON,
    toJSON,
 )
import Data.Char (isSpace)
import Data.Either (isRight)
import qualified Data.HashMap.Strict as Map
import Data.List (
    dropWhileEnd,
    find,
 )
import Data.Maybe (
    catMaybes,
    fromMaybe,
 )
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Development.IDE (
    GetModSummary (..),
    GhcSession (..),
    HscEnvEq (envImportPaths, hscEnv),
    IdeState,
    List (List),
    NormalizedFilePath,
    Range (Range),
    Uri,
    evalGhcEnv,
    hscEnvWithImportPaths,
    runAction,
    stringBufferToByteString,
    textToStringBuffer,
    toNormalizedFilePath',
    toNormalizedUri,
    uriToFilePath',
    use_,
 )
import Development.IDE.Core.Preprocessor (
    preprocessor,
 )
import Development.IDE.GHC.Compat (HscEnv)
import DynamicLoading (initializePlugins)
import GHC (
    ExecOptions (
        execLineNumber,
        execSourceFile
    ),
    ExecResult (..),
    GeneralFlag (..),
    Ghc,
    GhcLink (LinkInMemory),
    GhcMode (CompManager),
    GhcMonad (getSession),
    HscTarget (HscInterpreted),
    LoadHowMuch (LoadAllTargets),
    ModSummary (ms_hspp_opts),
    Module (moduleName),
    SuccessFlag (Failed, Succeeded),
    TcRnExprMode (..),
    execOptions,
    execStmt,
    exprType,
    getInteractiveDynFlags,
    getSessionDynFlags,
    isImport,
    isStmt,
    load,
    runDecls,
    setContext,
    setInteractiveDynFlags,
    setLogAction,
    setSessionDynFlags,
    setTargets,
    typeKind,
 )
import GHC.Generics (Generic)
import qualified GHC.LanguageExtensions.Type as LangExt
import GhcPlugins (
    DynFlags (..),
    defaultLogActionHPutStrDoc,
    gopt_set,
    gopt_unset,
    interpWays,
    targetPlatform,
    updateWays,
    wayGeneralFlags,
    wayUnsetGeneralFlags,
    xopt_set,
 )
import HscTypes (
    InteractiveImport (IIModule),
    ModSummary (ms_mod),
    Target (Target),
    TargetId (TargetFile),
 )
import Ide.Plugin (mkLspCommand)
import Ide.Plugin.Eval.Code (
    Statement,
    asStatements,
    evalExpr,
    evalExtensions,
    evalSetup,
    propSetup,
    resultRange,
    testCheck,
    testRanges,
 )
import Ide.Plugin.Eval.GHC (
    addExtension,
    addImport,
    addPackages,
    hasPackage,
    isExpr,
    showDynFlags,
 )
import Ide.Plugin.Eval.Parse.Option (langOptions)
import Ide.Plugin.Eval.Parse.Section (
    Section (
        sectionFormat,
        sectionTests
    ),
    allSections,
 )
import Ide.Plugin.Eval.Parse.Token (tokensFrom)
import Ide.Plugin.Eval.Types (
    Format (SingleLine),
    Loc,
    Located (Located),
    Test,
    hasTests,
    isProperty,
    splitSections,
    unLoc,
 )
import Ide.Plugin.Eval.Util (
    asS,
    gStrictTry,
    handleMaybe,
    handleMaybeM,
    isLiterate,
    logWith,
    response,
    response',
    timed,
 )
import Ide.Types (
    CodeLensProvider,
    CommandFunction,
    CommandId,
    PluginCommand (PluginCommand),
 )
import Language.Haskell.LSP.Core (
    LspFuncs (
        getVirtualFileFunc,
        withIndefiniteProgress
    ),
    ProgressCancellable (
        Cancellable
    ),
 )
import Language.Haskell.LSP.Types (
    ApplyWorkspaceEditParams (
        ApplyWorkspaceEditParams
    ),
    CodeLens (CodeLens),
    CodeLensParams (
        CodeLensParams,
        _textDocument
    ),
    Command (_arguments, _title),
    ServerMethod (
        WorkspaceApplyEdit
    ),
    TextDocumentIdentifier (..),
    TextEdit (TextEdit),
    WorkspaceEdit (WorkspaceEdit),
 )
import Language.Haskell.LSP.VFS (virtualFileText)
import Outputable (
    nest,
    ppr,
    showSDoc,
    text,
    ($$),
    (<+>),
 )
import System.FilePath (takeFileName)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Text.Read (readMaybe)
import Util (OverridingBool (Never))

{- | Code Lens provider
 NOTE: Invoked every time the document is modified, not just when the document is saved.
-}
codeLens :: CodeLensProvider
codeLens lsp st plId CodeLensParams{_textDocument} =
    let dbg = logWith st
        perf = timed dbg
     in perf "codeLens" $
            response $ do
                let TextDocumentIdentifier uri = _textDocument
                fp <- handleMaybe "uri" $ uriToFilePath' uri
                dbg "fp" fp
                mdlText <- moduleText lsp uri

                {- Normalise CPP/LHS files/custom preprocessed files.
                   Used to extract tests correctly from CPP and LHS (Bird-style).
                -}
                session :: HscEnvEq <-
                    runGetSession st $ toNormalizedFilePath' fp

                Right (ppContent, _dflags) <-
                    perf "preprocessor" $
                        liftIO $
                            runExceptT $
                                preprocessor (hscEnv session) fp (Just $ textToStringBuffer mdlText)
                let text =
                        cleanSource (isLiterate fp) . decodeUtf8 $
                            stringBufferToByteString
                                ppContent
                -- dbg "PREPROCESSED CONTENT" text

                -- Extract tests from source code
                let Right (setups, nonSetups) =
                        (splitSections . filter hasTests <$>)
                            . allSections
                            . tokensFrom
                            . T.unpack
                            $ text
                let tests = testsBySection nonSetups

                cmd <- liftIO $ mkLspCommand plId evalCommandName "Evaluate=..." (Just [])
                let lenses =
                        [ CodeLens testRange (Just cmd') Nothing
                        | (section, test) <- tests
                        , let (testRange, resultRange) = testRanges test
                              args = EvalParams (setups ++ [section]) _textDocument
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
                            , show (length nonSetups)
                            , "sections"
                            , show (length setups)
                            , "setups"
                            , show (length lenses)
                            , "lenses."
                            ]

                return $ List lenses
  where
    trivial (Range p p') = p == p'

evalCommandName :: CommandId
evalCommandName = "evalCommand"

evalCommand :: PluginCommand
evalCommand = PluginCommand evalCommandName "evaluate" runEvalCmd

-- |Specify the test section to execute
data EvalParams = EvalParams
    { sections :: [Section]
    , module_ :: !TextDocumentIdentifier
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

runEvalCmd :: CommandFunction EvalParams
runEvalCmd lsp st EvalParams{..} =
    let dbg = logWith st
        perf = timed dbg
        cmd = do
            let tests = testsBySection sections

            let TextDocumentIdentifier{_uri} = module_
            fp <- handleMaybe "uri" $ uriToFilePath' _uri
            let nfp = toNormalizedFilePath' fp
            mdlText <- moduleText lsp _uri

            session <- runGetSession st nfp

            (ms, _) <-
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
            hscEnv' <- withSystemTempFile (takeFileName fp) $ \logFilename logHandle -> ExceptT . (either Left id <$>) . gStrictTry . evalGhcEnv (hscEnvWithImportPaths session) $ do
                env <- getSession

                -- Install the module pragmas and options
                df <- liftIO $ setupDynFlagsForGHCiLike env $ ms_hspp_opts ms

                let impPaths = fromMaybe (importPaths df) (envImportPaths session)
                -- Restore the cradle import paths
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

            let workspaceEditsMap = Map.fromList [(_uri, List edits)]
            let workspaceEdits = WorkspaceEdit (Just workspaceEditsMap) Nothing

            return (WorkspaceApplyEdit, ApplyWorkspaceEditParams workspaceEdits)
     in perf "evalCmd" $
            withIndefiniteProgress lsp "Evaluating" Cancellable $
                response' cmd

moduleText :: (IsString e, MonadIO m) => LspFuncs c -> Uri -> ExceptT e m Text
moduleText lsp uri =
    handleMaybeM "mdlText" $
        liftIO $
            (virtualFileText <$>)
                <$> getVirtualFileFunc
                    lsp
                    (toNormalizedUri uri)

testsBySection :: [Section] -> [(Section, Loc Test)]
testsBySection sections =
    [(section, test) | section <- sections, test <- sectionTests section]

type TEnv = (IdeState, String)

runTests :: TEnv -> [(Section, Loc Test)] -> Ghc [TextEdit]
runTests e@(_st, _) tests = do
    df <- getInteractiveDynFlags
    evalSetup
    when (hasQuickCheck df && needsQuickCheck tests) $ void $ evals e df propSetup

    mapM (processTest e df) tests
  where
    processTest :: TEnv -> DynFlags -> (Section, Loc Test) -> Ghc TextEdit
    processTest e@(st, fp) df (section, test) = do
        let dbg = logWith st
        let pad = pad_ $ (if isLiterate fp then ("> " `T.append`) else id) $ padPrefix (sectionFormat section)

        rs <- runTest e df test
        dbg "TEST RESULTS" rs

        let checkedResult = testCheck (section, unLoc test) rs

        let edit = TextEdit (resultRange test) (T.unlines . map pad $ checkedResult)
        dbg "TEST EDIT" edit
        return edit

    -- runTest :: String -> DynFlags -> Loc Test -> Ghc [Text]
    runTest _ df test
        | not (hasQuickCheck df) && (isProperty . unLoc $ test) =
            return $
                singleLine
                    "Add QuickCheck to your cabal dependencies to run this test."
    runTest e df test = evals e df (asStatements test)

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
Variable not in scope: cls :: t0 -> f0
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
        | -- A :set -XLanguageOption directive
          isRight (langOptions stmt) =
            either
                (return . Just . errorLines)
                ( \es -> do
                    dbg "{:SET" es
                    ndf <- getInteractiveDynFlags
                    dbg "pre set" $ showDynFlags ndf
                    mapM_ addExtension es
                    ndf <- getInteractiveDynFlags
                    dbg "post set" $ showDynFlags ndf
                    return Nothing
                )
                $ ghcOptions stmt
        | -- A type/kind command
          Just (cmd, arg) <- parseGhciLikeCmd $ T.pack stmt =
            evalGhciLikeCmd cmd arg
        | -- An expression
          isExpr df stmt =
            do
                dbg "{EXPR" stmt
                eres <- gStrictTry $ evalExpr stmt
                dbg "RES ->" eres
                let res = case eres of
                        Left err -> errorLines err
                        Right rs -> [T.pack rs]
                dbg "EXPR} ->" res
                return . Just $ res
        | -- A statement
          isStmt df stmt =
            do
                dbg "{STMT " stmt
                res <- exec stmt l
                r <- case res of
                    ExecComplete (Left err) _ -> return . Just . errorLines . show $ err
                    ExecComplete (Right _) _ -> return Nothing
                    ExecBreak{} ->
                        return . Just . singleLine $ "breakpoints are not supported"
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
         in execStmt stmt opts

runGetSession :: MonadIO m => IdeState -> NormalizedFilePath -> m HscEnvEq
runGetSession st nfp =
    liftIO $
        runAction "getSession" st $
            use_
                GhcSession
                -- GhcSessionDeps
                nfp

needsQuickCheck :: [(Section, Loc Test)] -> Bool
needsQuickCheck = any (isProperty . unLoc . snd)

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
    map (\e -> fromMaybe e (T.stripSuffix "arising from a use of ‘asPrint’" e))
        . dropWhileEnd T.null
        . takeWhile (not . ("CallStack" `T.isPrefixOf`))
        . T.lines
        . T.pack

{-
Check that extensions actually exists.

>>> ghcOptions ":set -XLambdaCase"
Right [LambdaCase]
>>> ghcOptions ":set -XLambdaCase -XNotRight"
Left "Unknown extension: \"NotRight\""
-}
ghcOptions :: [Char] -> Either String [LangExt.Extension]
ghcOptions = either Left (mapM chk) . langOptions
  where
    chk o =
        maybe
            (Left $ unwords ["Unknown extension:", show o])
            Right
            (readMaybe o :: Maybe LangExt.Extension)

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
padPrefix _ = ""

{-
Normalise preprocessed source code (from a CPP/LHS or other processed file) so that tests are on the same lines as in the original source.

>>> cleanSource True $ T.pack "#line 1 \nA comment\n> module X where"
"comment\nmodule X where\n"

>>> cleanSource False $ T.pack "#1  \nmodule X where"
"module X where\n"
-}
cleanSource :: Bool -> Text -> Text
cleanSource isLit =
    T.unlines
        . reverse
        . (if isLit then map cleanBirdCode else id)
        . takeWhile (\t -> T.null t || (T.head t /= '#'))
        . reverse
        . T.lines

cleanBirdCode :: Text -> Text
cleanBirdCode = T.drop 2

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
    _ -> (TM_Inst, rawArg)

data GhciLikeCmdException = GhciLikeCmdNotImplemented
    { ghciCmdName :: Text
    , ghciCmdArg :: Text
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
