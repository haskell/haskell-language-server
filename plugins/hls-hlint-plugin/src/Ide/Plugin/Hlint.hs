{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

-- On 9.4 we get a new redundant constraint warning, but deleting the
-- constraint breaks the build on earlier versions. Rather than apply
-- lots of CPP, we just disable the warning until later.
{-# OPTIONS_GHC -Wno-redundant-constraints   #-}

#ifdef HLINT_ON_GHC_LIB
#define MIN_GHC_API_VERSION(x,y,z) MIN_VERSION_ghc_lib_parser(x,y,z)
#else
#define MIN_GHC_API_VERSION(x,y,z) MIN_VERSION_ghc(x,y,z)
#endif

module Ide.Plugin.Hlint
  (
    descriptor
  , Log(..)
  ) where
import           Control.Arrow                                      ((&&&))
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens                                       ((?~), (^.))
import           Control.Monad
import           Control.Monad.Error.Class                          (MonadError (throwError))
import           Control.Monad.IO.Class                             (MonadIO (liftIO))
import           Control.Monad.Trans.Class                          (MonadTrans (lift))
import           Control.Monad.Trans.Except                         (ExceptT (..),
                                                                     runExceptT)
import           Data.Aeson.Types                                   (FromJSON (..),
                                                                     ToJSON (..),
                                                                     Value (..))
import qualified Data.ByteString                                    as BS
import           Data.Hashable
import qualified Data.HashMap.Strict                                as Map
import qualified Data.Map                                           as M
import           Data.Maybe
import qualified Data.Text                                          as T
import qualified Data.Text.Encoding                                 as T
import           Data.Typeable
import           Development.IDE                                    hiding
                                                                    (Error,
                                                                     getExtensions)
import           Development.IDE.Core.Compile                       (sourceParser)
import           Development.IDE.Core.Rules                         (defineNoFile,
                                                                     getParsedModuleWithComments)
import           Development.IDE.Core.Shake                         (getDiagnostics)
import qualified Refact.Apply                                       as Refact
import qualified Refact.Types                                       as Refact

#ifdef HLINT_ON_GHC_LIB
import           Development.IDE.GHC.Compat                         (DynFlags,
                                                                     WarningFlag (Opt_WarnUnrecognisedPragmas),
                                                                     extensionFlags,
                                                                     ms_hspp_opts,
                                                                     topDir,
                                                                     wopt)
import qualified Development.IDE.GHC.Compat.Util                    as EnumSet

#if MIN_GHC_API_VERSION(9,4,0)
import qualified "ghc-lib-parser" GHC.Data.Strict                   as Strict
#endif
#if MIN_GHC_API_VERSION(9,0,0)
import           "ghc-lib-parser" GHC.Types.SrcLoc                  hiding
                                                                    (RealSrcSpan)
import qualified "ghc-lib-parser" GHC.Types.SrcLoc                  as GHC
#else
import           "ghc-lib-parser" SrcLoc                            hiding
                                                                    (RealSrcSpan)
import qualified "ghc-lib-parser" SrcLoc                            as GHC
#endif
import           "ghc-lib-parser" GHC.LanguageExtensions            (Extension)
import           Language.Haskell.GhclibParserEx.GHC.Driver.Session as GhclibParserEx (readExtension)
import           System.FilePath                                    (takeFileName)
import           System.IO                                          (IOMode (WriteMode),
                                                                     hClose,
                                                                     hPutStr,
                                                                     hSetEncoding,
                                                                     hSetNewlineMode,
                                                                     noNewlineTranslation,
                                                                     utf8,
                                                                     withFile)
import           System.IO.Temp
#else
import           Development.IDE.GHC.Compat                         hiding
                                                                    (setEnv,
                                                                     (<+>))
import           GHC.Generics                                       (Associativity (LeftAssociative, NotAssociative, RightAssociative))
#if MIN_GHC_API_VERSION(9,2,0)
import           Language.Haskell.GHC.ExactPrint.ExactPrint         (deltaOptions)
#else
import           Language.Haskell.GHC.ExactPrint.Delta              (deltaOptions)
#endif
import           Language.Haskell.GHC.ExactPrint.Parsers            (postParseTransform)
import           Language.Haskell.GHC.ExactPrint.Types              (Rigidity (..))
import           Language.Haskell.GhclibParserEx.Fixity             as GhclibParserEx (applyFixities)
import qualified Refact.Fixity                                      as Refact
#endif
import           Ide.Plugin.Config                                  hiding
                                                                    (Config)
import           Ide.Plugin.Error
import           Ide.Plugin.Properties
import           Ide.Plugin.Resolve
import           Ide.PluginUtils
import           Ide.Types                                          hiding
                                                                    (Config)
import           Language.Haskell.HLint                             as Hlint hiding
                                                                             (Error)
import qualified Language.LSP.Protocol.Lens                         as LSP
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types                        hiding
                                                                    (Null)
import qualified Language.LSP.Protocol.Types                        as LSP
import           Language.LSP.Server                                (getVersionedTextDoc)

import qualified Development.IDE.Core.Shake                         as Shake
import           Development.IDE.Spans.Pragmas                      (LineSplitTextEdits (LineSplitTextEdits),
                                                                     NextPragmaInfo (NextPragmaInfo),
                                                                     getNextPragmaInfo,
                                                                     lineSplitDeleteTextEdit,
                                                                     lineSplitInsertTextEdit,
                                                                     lineSplitTextEdits,
                                                                     nextPragmaLine)
import           GHC.Generics                                       (Generic)
#if MIN_VERSION_apply_refact(0,12,0)
#else
import           System.Environment                                 (setEnv,
                                                                     unsetEnv)
#endif
import           Development.IDE.Core.PluginUtils                   as PluginUtils
import           Text.Regex.TDFA.Text                               ()
-- ---------------------------------------------------------------------

data Log
  = LogShake Shake.Log
  | LogApplying NormalizedFilePath (Either String WorkspaceEdit)
  | LogGeneratedIdeas NormalizedFilePath [[Refact.Refactoring Refact.SrcSpan]]
  | LogGetIdeas NormalizedFilePath
  | LogUsingExtensions NormalizedFilePath [String] -- Extension is only imported conditionally, so we just stringify them
  | forall a. (Pretty a) => LogResolve a

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log
    LogApplying fp res -> "Applying hint(s) for" <+> viaShow fp <> ":" <+> viaShow res
    LogGeneratedIdeas fp ideas -> "Generated hlint ideas for for" <+> viaShow fp <> ":" <+> viaShow ideas
    LogUsingExtensions fp exts -> "Using extensions for " <+> viaShow fp <> ":" <+> pretty exts
    LogGetIdeas fp -> "Getting hlint ideas for " <+> viaShow fp
    LogResolve msg -> pretty msg

#ifdef HLINT_ON_GHC_LIB
-- Reimplementing this, since the one in Development.IDE.GHC.Compat isn't for ghc-lib
#if !MIN_GHC_API_VERSION(9,0,0)
type BufSpan = ()
#endif
pattern RealSrcSpan :: GHC.RealSrcSpan -> Maybe BufSpan -> GHC.SrcSpan
#if MIN_GHC_API_VERSION(9,4,0)
pattern RealSrcSpan x y <- GHC.RealSrcSpan x (fromStrictMaybe -> y)
#elif MIN_GHC_API_VERSION(9,0,0)
pattern RealSrcSpan x y = GHC.RealSrcSpan x y
#else
pattern RealSrcSpan x y <- ((,Nothing) -> (GHC.RealSrcSpan x, y))
#endif
{-# COMPLETE RealSrcSpan, UnhelpfulSpan #-}
#endif

#if MIN_GHC_API_VERSION(9,4,0)
fromStrictMaybe :: Strict.Maybe a -> Maybe a
fromStrictMaybe (Strict.Just a ) = Just a
fromStrictMaybe  Strict.Nothing  = Nothing
#endif

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  let resolveRecorder = cmapWithPrio LogResolve recorder
      (pluginCommands, pluginHandlers) = mkCodeActionWithResolveAndCommand resolveRecorder plId codeActionProvider (resolveProvider recorder)
      desc = "Provides HLint diagnostics and code actions. Built with hlint-" <> VERSION_hlint
  in (defaultPluginDescriptor plId desc)
  { pluginRules = rules recorder plId
  , pluginCommands = pluginCommands
  , pluginHandlers = pluginHandlers
  , pluginConfigDescriptor = defaultConfigDescriptor
      { configHasDiagnostics = True
      , configCustomConfig = mkCustomConfig properties
      }
  }

-- This rule only exists for generating file diagnostics
-- so the RuleResult is empty
data GetHlintDiagnostics = GetHlintDiagnostics
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHlintDiagnostics
instance NFData   GetHlintDiagnostics

type instance RuleResult GetHlintDiagnostics = ()

-- | Hlint rules to generate file diagnostics based on hlint hints
-- | This rule is recomputed when:
-- | - A file has been edited via
-- |    - `getIdeas` -> `getParsedModule` in any case
-- |    - `getIdeas` -> `getFileContents` if the hls ghc does not match the hlint default ghc
-- | - The client settings have changed, to honour the `hlintOn` setting, via `getClientConfigAction`
-- | - The hlint specific settings have changed, via `getHlintSettingsRule`
rules :: Recorder (WithPriority Log) -> PluginId -> Rules ()
rules recorder plugin = do
  define (cmapWithPrio LogShake recorder) $ \GetHlintDiagnostics file -> do
    config <- getPluginConfigAction plugin
    let hlintOn = plcGlobalOn config && plcDiagnosticsOn config
    ideas <- if hlintOn then getIdeas recorder file else return (Right [])
    return (diagnostics file ideas, Just ())

  defineNoFile (cmapWithPrio LogShake recorder) $ \GetHlintSettings -> do
    (Config flags) <- getHlintConfig plugin
    liftIO $ argsSettings flags

  action $ do
    files <- getFilesOfInterestUntracked
    void $ uses GetHlintDiagnostics $ Map.keys files

  where

      diagnostics :: NormalizedFilePath -> Either ParseError [Idea] -> [FileDiagnostic]
      diagnostics file (Right ideas) =
        [(file, ShowDiag, ideaToDiagnostic i) | i <- ideas, ideaSeverity i /= Ignore]
      diagnostics file (Left parseErr) =
        [(file, ShowDiag, parseErrorToDiagnostic parseErr)]

      ideaToDiagnostic :: Idea -> Diagnostic
      ideaToDiagnostic idea =
        LSP.Diagnostic {
            _range    = srcSpanToRange $ ideaSpan idea
          , _severity = Just LSP.DiagnosticSeverity_Information
          -- we are encoding the fact that idea has refactorings in diagnostic code
          , _code     = Just (InR $ T.pack $ codePre ++ ideaHint idea)
          , _source   = Just "hlint"
          , _message  = idea2Message idea
          , _relatedInformation = Nothing
          , _tags     = Nothing
          , _codeDescription = Nothing
          , _data_ = Nothing
        }
        where codePre = if null $ ideaRefactoring idea then "" else "refact:"

      idea2Message :: Idea -> T.Text
      idea2Message idea = T.unlines $ [T.pack $ ideaHint idea, "Found:", "  " <> T.pack (ideaFrom idea)]
                                     <> toIdea <> map (T.pack . show) (ideaNote idea)
        where
          toIdea :: [T.Text]
          toIdea = case ideaTo idea of
            Nothing -> []
            Just i  -> [T.pack "Why not:", T.pack $ "  " ++ i]


      parseErrorToDiagnostic :: ParseError -> Diagnostic
      parseErrorToDiagnostic (Hlint.ParseError l msg contents) =
        LSP.Diagnostic {
            _range    = srcSpanToRange l
          , _severity = Just LSP.DiagnosticSeverity_Information
          , _code     = Just (InR sourceParser)
          , _source   = Just "hlint"
          , _message  = T.unlines [T.pack msg,T.pack contents]
          , _relatedInformation = Nothing
          , _tags     = Nothing
          , _codeDescription = Nothing
          , _data_ = Nothing
        }

      -- This one is defined in Development.IDE.GHC.Error but here
      -- the types could come from ghc-lib or ghc
      srcSpanToRange :: SrcSpan -> LSP.Range
      srcSpanToRange (RealSrcSpan span _) = Range {
          _start = LSP.Position {
                _line = fromIntegral $ srcSpanStartLine span - 1
              , _character  = fromIntegral $ srcSpanStartCol span - 1}
        , _end   = LSP.Position {
                _line = fromIntegral $ srcSpanEndLine span - 1
             , _character = fromIntegral $ srcSpanEndCol span - 1}
        }
      srcSpanToRange (UnhelpfulSpan _) = noRange

getIdeas :: Recorder (WithPriority Log) -> NormalizedFilePath -> Action (Either ParseError [Idea])
getIdeas recorder nfp = do
  logWith recorder Debug $ LogGetIdeas nfp
  (flags, classify, hint) <- useNoFile_ GetHlintSettings

  let applyHints' (Just (Right modEx)) = Right $ applyHints classify hint [modEx]
      applyHints' (Just (Left err)) = Left err
      applyHints' Nothing = Right []

  fmap applyHints' (moduleEx flags)

  where moduleEx :: ParseFlags -> Action (Maybe (Either ParseError ModuleEx))
#ifndef HLINT_ON_GHC_LIB
        moduleEx _flags = do
          mbpm <- getParsedModuleWithComments nfp
          return $ createModule <$> mbpm
          where
            createModule pm = Right (createModuleEx anns (applyParseFlagsFixities modu))
                  where anns = pm_annotations pm
                        modu = pm_parsed_source pm

            applyParseFlagsFixities :: ParsedSource -> ParsedSource
            applyParseFlagsFixities modul = GhclibParserEx.applyFixities (parseFlagsToFixities _flags) modul

            parseFlagsToFixities :: ParseFlags -> [(String, Fixity)]
            parseFlagsToFixities = map toFixity . Hlint.fixities

            toFixity :: FixityInfo -> (String, Fixity)
            toFixity (name, dir, i) = (name, Fixity NoSourceText i $ f dir)
                where
                    f LeftAssociative  = InfixL
                    f RightAssociative = InfixR
                    f NotAssociative   = InfixN
#else
        moduleEx flags = do
          mbpm <- getParsedModuleWithComments nfp
          -- If ghc was not able to parse the module, we disable hlint diagnostics
          if isNothing mbpm
              then return Nothing
              else do
                     flags' <- setExtensions flags
                     (_, contents) <- getFileContents nfp
                     let fp = fromNormalizedFilePath nfp
                     let contents' = T.unpack <$> contents
                     Just <$> liftIO (parseModuleEx flags' fp contents')

        setExtensions flags = do
          hlintExts <- getExtensions nfp
          logWith recorder Debug $ LogUsingExtensions nfp (fmap show hlintExts)
          return $ flags { enabledExtensions = hlintExts }

-- Gets extensions from ModSummary dynflags for the file.
-- Previously this would union extensions from both hlint's parsedFlags
-- and the ModSummary dynflags. However using the parsedFlags extensions
-- can sometimes interfere with the hlint parsing of the file.
-- See https://github.com/haskell/haskell-language-server/issues/1279
--
-- Note: this is used when HLINT_ON_GHC_LIB is defined. We seem to need
-- these extensions to construct dynflags to parse the file again. Therefore
-- using hlint default extensions doesn't seem to be a problem when
-- HLINT_ON_GHC_LIB is not defined because we don't parse the file again.
getExtensions :: NormalizedFilePath -> Action [Extension]
getExtensions nfp = do
    dflags <- getFlags
    let hscExts = EnumSet.toList (extensionFlags dflags)
    let hscExts' = mapMaybe (GhclibParserEx.readExtension . show) hscExts
    return hscExts'
  where getFlags :: Action DynFlags
        getFlags = do
          modsum <- use_ GetModSummary nfp
          return $ ms_hspp_opts $ msrModSummary modsum
#endif

-- ---------------------------------------------------------------------

data GetHlintSettings = GetHlintSettings
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHlintSettings
instance NFData   GetHlintSettings
instance NFData Hint where rnf = rwhnf
instance NFData Classify where rnf = rwhnf
instance NFData ParseFlags where rnf = rwhnf
instance Show Hint where show = const "<hint>"
instance Show ParseFlags where show = const "<parseFlags>"

type instance RuleResult GetHlintSettings = (ParseFlags, [Classify], Hint)

-- ---------------------------------------------------------------------

newtype Config = Config [String]

properties :: Properties '[ 'PropertyKey "flags" ('TArray String)]
properties = emptyProperties
  & defineArrayProperty #flags
    "Flags used by hlint" []

-- | Get the plugin config
getHlintConfig :: PluginId -> Action Config
getHlintConfig pId =
  Config
    <$> usePropertyAction #flags pId properties

-- ---------------------------------------------------------------------
codeActionProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeActionProvider ideState _pluginId (CodeActionParams _ _ documentId _ context)
  | let TextDocumentIdentifier uri = documentId
  , Just docNormalizedFilePath <- uriToNormalizedFilePath (toNormalizedUri uri)
  = do
    verTxtDocId <- lift $ getVersionedTextDoc documentId
    liftIO $ fmap (InL . map LSP.InR) $ do
      allDiagnostics <- atomically $ getDiagnostics ideState

      let numHintsInDoc = length
            [diagnostic | (diagnosticNormalizedFilePath, _, diagnostic) <- allDiagnostics
                        , validCommand diagnostic
                        , diagnosticNormalizedFilePath == docNormalizedFilePath
            ]
      let numHintsInContext = length
            [diagnostic | diagnostic <- diags
                        , validCommand diagnostic
            ]
      let singleHintCodeActions = diags >>= diagnosticToCodeActions verTxtDocId
      if numHintsInDoc > 1 && numHintsInContext > 0 then do
        pure $ singleHintCodeActions ++ [applyAllAction verTxtDocId]
      else
        pure singleHintCodeActions
  | otherwise
  = pure $ InL []

  where
    applyAllAction verTxtDocId =
      let args = Just $ toJSON (ApplyHint verTxtDocId Nothing)
        in LSP.CodeAction "Apply all hints" (Just LSP.CodeActionKind_QuickFix) Nothing Nothing Nothing Nothing Nothing args

    -- |Some hints do not have an associated refactoring
    validCommand (LSP.Diagnostic _ _ (Just (InR code)) _ (Just "hlint") _ _ _ _) =
        "refact:" `T.isPrefixOf` code
    validCommand _ =
        False

    diags = context ^. LSP.diagnostics

resolveProvider :: Recorder (WithPriority Log) -> ResolveFunction IdeState HlintResolveCommands Method_CodeActionResolve
resolveProvider recorder ideState _plId ca uri resolveValue = do
  file <-  getNormalizedFilePathE uri
  case resolveValue of
    (ApplyHint verTxtDocId oneHint) -> do
        edit <- ExceptT $ liftIO $ applyHint recorder ideState file oneHint verTxtDocId
        pure $ ca & LSP.edit ?~ edit
    (IgnoreHint verTxtDocId hintTitle ) -> do
        edit <- ExceptT $ liftIO $ ignoreHint recorder ideState file verTxtDocId hintTitle
        pure $ ca & LSP.edit ?~ edit

-- | Convert a hlint diagnostic into an apply and an ignore code action
-- if applicable
diagnosticToCodeActions :: VersionedTextDocumentIdentifier -> LSP.Diagnostic -> [LSP.CodeAction]
diagnosticToCodeActions verTxtDocId diagnostic
  | LSP.Diagnostic{ _source = Just "hlint", _code = Just (InR code), _range = LSP.Range start _ } <- diagnostic
  , let isHintApplicable = "refact:" `T.isPrefixOf` code
  , let hint = T.replace "refact:" "" code
  , let suppressHintTitle = "Ignore hint \"" <> hint <> "\" in this module"
  , let suppressHintArguments = IgnoreHint verTxtDocId hint
  = catMaybes
      -- Applying the hint is marked preferred because it addresses the underlying error.
      -- Disabling the rule isn't, because less often used and configuration can be adapted.
      [ if | isHintApplicable
           , let applyHintTitle = "Apply hint \"" <> hint <> "\""
                 applyHintArguments = ApplyHint verTxtDocId (Just $ OneHint start hint) ->
               Just (mkCodeAction applyHintTitle diagnostic (Just (toJSON applyHintArguments)) True)
           | otherwise -> Nothing
      , Just (mkCodeAction suppressHintTitle diagnostic (Just (toJSON suppressHintArguments)) False)
      ]
  | otherwise = []

mkCodeAction :: T.Text -> LSP.Diagnostic -> Maybe Value -> Bool -> LSP.CodeAction
mkCodeAction title diagnostic data_  isPreferred =
  LSP.CodeAction
    { _title = title
    , _kind = Just LSP.CodeActionKind_QuickFix
    , _diagnostics = Just [diagnostic]
    , _isPreferred = Just isPreferred
    , _disabled = Nothing
    , _edit = Nothing
    , _command = Nothing
    , _data_ = data_
    }

mkSuppressHintTextEdits :: DynFlags -> T.Text -> T.Text -> [LSP.TextEdit]
mkSuppressHintTextEdits dynFlags fileContents hint =
  let
    NextPragmaInfo{ nextPragmaLine, lineSplitTextEdits } = getNextPragmaInfo dynFlags (Just fileContents)
    nextPragmaLinePosition = Position (fromIntegral nextPragmaLine) 0
    nextPragmaRange = Range nextPragmaLinePosition nextPragmaLinePosition
    wnoUnrecognisedPragmasText =
      if wopt Opt_WarnUnrecognisedPragmas dynFlags
      then Just "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}\n"
      else Nothing
    hlintIgnoreText = Just ("{-# HLINT ignore \"" <> hint <> "\" #-}\n")
    -- we combine the texts into a single text because lsp-test currently
    -- applies text edits backwards and I want the options pragma to
    -- appear above the hlint pragma in the tests
    combinedText = mconcat $ catMaybes [wnoUnrecognisedPragmasText, hlintIgnoreText]
    combinedTextEdit = LSP.TextEdit nextPragmaRange combinedText
    lineSplitTextEditList = maybe [] (\LineSplitTextEdits{..} -> [lineSplitInsertTextEdit, lineSplitDeleteTextEdit]) lineSplitTextEdits
  in
    combinedTextEdit : lineSplitTextEditList
-- ---------------------------------------------------------------------

ignoreHint :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> VersionedTextDocumentIdentifier -> HintTitle -> IO (Either PluginError WorkspaceEdit)
ignoreHint _recorder ideState nfp verTxtDocId ignoreHintTitle = runExceptT $ do
  (_, fileContents) <- runActionE "Hlint.GetFileContents" ideState $ useE GetFileContents nfp
  (msr, _) <- runActionE "Hlint.GetModSummaryWithoutTimestamps" ideState $ useWithStaleE GetModSummaryWithoutTimestamps nfp
  case fileContents of
    Just contents -> do
        let dynFlags = ms_hspp_opts $ msrModSummary msr
            textEdits = mkSuppressHintTextEdits dynFlags contents ignoreHintTitle
            workspaceEdit =
                LSP.WorkspaceEdit
                  (Just (M.singleton (verTxtDocId ^. LSP.uri) textEdits))
                  Nothing
                  Nothing
        pure workspaceEdit
    Nothing -> throwError $ PluginInternalError "Unable to get fileContents"

-- ---------------------------------------------------------------------
data HlintResolveCommands =
    ApplyHint
      { verTxtDocId :: VersionedTextDocumentIdentifier
      -- |If Nothing, apply all hints, otherise only apply
      -- the given hint
      , oneHint     :: Maybe OneHint
      }
  | IgnoreHint
      { verTxtDocId     :: VersionedTextDocumentIdentifier
      , ignoreHintTitle :: HintTitle
      } deriving (Generic, ToJSON, FromJSON)

type HintTitle = T.Text

data OneHint =
  OneHint
    { oneHintPos   :: Position
    , oneHintTitle :: HintTitle
    } deriving (Generic, Eq, Show, ToJSON, FromJSON)

applyHint :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> Maybe OneHint -> VersionedTextDocumentIdentifier -> IO (Either PluginError WorkspaceEdit)
applyHint recorder ide nfp mhint verTxtDocId =
  runExceptT $ do
    let runAction' :: Action a -> IO a
        runAction' = runAction "applyHint" ide
    let errorHandlers = [ Handler $ \e -> return (Left (show (e :: IOException)))
                        , Handler $ \e -> return (Left (show (e :: ErrorCall)))
                        ]
    ideas <- bimapExceptT (PluginInternalError . T.pack . showParseError) id $ ExceptT $ runAction' $ getIdeas recorder nfp
    let ideas' = maybe ideas (`filterIdeas` ideas) mhint
    let commands = map ideaRefactoring ideas'
    logWith recorder Debug $ LogGeneratedIdeas nfp commands
    let fp = fromNormalizedFilePath nfp
    (_, mbOldContent) <- liftIO $ runAction' $ getFileContents nfp
    oldContent <- maybe (liftIO $ fmap T.decodeUtf8 (BS.readFile fp)) return mbOldContent
    modsum <- liftIO $ runAction' $ use_ GetModSummary nfp
    let dflags = ms_hspp_opts $ msrModSummary modsum

    -- set Nothing as "position" for "applyRefactorings" because
    -- applyRefactorings expects the provided position to be _within_ the scope
    -- of each refactoring it will apply.
    -- But "Idea"s returned by HLint point to starting position of the expressions
    -- that contain refactorings, so they are often outside the refactorings' boundaries.
    let position = Nothing
#ifdef HLINT_ON_GHC_LIB
    let writeFileUTF8NoNewLineTranslation file txt =
            withFile file WriteMode $ \h -> do
                hSetEncoding h utf8
                hSetNewlineMode h noNewlineTranslation
                hPutStr h (T.unpack txt)
    res <-
        liftIO $ withSystemTempFile (takeFileName fp) $ \temp h -> do
            hClose h
            writeFileUTF8NoNewLineTranslation temp oldContent
            exts <- runAction' $ getExtensions nfp
            -- We have to reparse extensions to remove the invalid ones
            let (enabled, disabled, _invalid) = Refact.parseExtensions $ map show exts
            let refactExts = map show $ enabled ++ disabled
            (Right <$> applyRefactorings (topDir dflags) position commands temp refactExts)
                `catches` errorHandlers
#else
    mbParsedModule <- liftIO $ runAction' $ getParsedModuleWithComments nfp
    res <-
        case mbParsedModule of
            Nothing -> throwError "Apply hint: error parsing the module"
            Just pm -> do
                let anns = pm_annotations pm
                let modu = pm_parsed_source pm
                -- apply-refact uses RigidLayout
                let rigidLayout = deltaOptions RigidLayout
                (anns', modu') <-
                    ExceptT $ mapM (uncurry Refact.applyFixities)
                            $ postParseTransform (Right (anns, [], dflags, modu)) rigidLayout
                liftIO $ (Right <$> Refact.applyRefactorings' position commands anns' modu')
                            `catches` errorHandlers
#endif
    case res of
      Right appliedFile -> do
        let wsEdit = diffText' True (verTxtDocId, oldContent) (T.pack appliedFile) IncludeDeletions
        ExceptT $ return (Right wsEdit)
      Left err ->
        throwError $ PluginInternalError $ T.pack err
    where
          -- | If we are only interested in applying a particular hint then
          -- let's filter out all the irrelevant ideas
          filterIdeas :: OneHint -> [Idea] -> [Idea]
          filterIdeas (OneHint (Position l c) title) ideas =
            let title' = T.unpack title
                ideaPos = (srcSpanStartLine &&& srcSpanStartCol) . toRealSrcSpan . ideaSpan
            in filter (\i -> ideaHint i == title' && ideaPos i == (fromIntegral $ l+1, fromIntegral $ c+1)) ideas

          toRealSrcSpan (RealSrcSpan real _) = real
          toRealSrcSpan (UnhelpfulSpan x) = error $ "No real source span: " ++ show x

          showParseError :: Hlint.ParseError -> String
          showParseError (Hlint.ParseError location message content) =
            unlines [show location, message, content]

-- | Map over both failure and success.
bimapExceptT :: Functor m => (e -> f) -> (a -> b) -> ExceptT e m a -> ExceptT f m b
bimapExceptT f g (ExceptT m) = ExceptT (fmap h m) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)
{-# INLINE bimapExceptT #-}

-- ---------------------------------------------------------------------------
-- Apply-refact compatability, documentation copied from upstream apply-refact
-- ---------------------------------------------------------------------------

-- | Apply a set of refactorings as supplied by HLint
--
-- This compatibility function abstracts over https://github.com/mpickering/apply-refact/issues/133
-- for backwards compatability.
applyRefactorings ::
  -- | FilePath to [GHC's libdir](https://downloads.haskell.org/ghc/latest/docs/users_guide/using.html#ghc-flag---print-libdir).
  --
  -- It is possible to use @libdir@ from [ghc-paths package](https://hackage.haskell.org/package/ghc-paths), but note
  -- this will make it difficult to provide a binary distribution of your program.
  FilePath ->
  -- | Apply hints relevant to a specific position
  Maybe (Int, Int) ->
  -- | 'Refactoring's to apply. Each inner list corresponds to an HLint
  -- <https://hackage.haskell.org/package/hlint/docs/Language-Haskell-HLint.html#t:Idea Idea>.
  -- An @Idea@ may have more than one 'Refactoring'.
  --
  -- The @Idea@s are sorted in ascending order of starting location, and are applied
  -- in that order. If two @Idea@s start at the same location, the one with the larger
  -- source span comes first. An @Idea@ is filtered out (ignored) if there is an @Idea@
  -- prior to it which has an overlapping source span and is not filtered out.
  [[Refact.Refactoring Refact.SrcSpan]] ->
  -- | Target file
  FilePath ->
  -- | GHC extensions, e.g., @LambdaCase@, @NoStarIsType@. The list is processed from left
  -- to right. An extension (e.g., @StarIsType@) may be overridden later (e.g., by @NoStarIsType@).
  --
  -- These are in addition to the @LANGUAGE@ pragmas in the target file. When they conflict
  -- with the @LANGUAGE@ pragmas, pragmas win.
  [String] ->
  IO String
applyRefactorings =
#if MIN_VERSION_apply_refact(0,12,0)
  Refact.applyRefactorings
#else
  \libdir pos refacts fp exts -> withRuntimeLibdir libdir (Refact.applyRefactorings pos refacts fp exts)

  where
    -- Setting a environment variable with the libdir used by ghc-exactprint.
    -- It is a workaround for an error caused by the use of a hardcoded at compile time libdir
    -- in ghc-exactprint that makes dependent executables non portables.
    -- See https://github.com/alanz/ghc-exactprint/issues/96.
    -- WARNING: this code is not thread safe, so if you try to apply several async refactorings
    -- it could fail. That case is not very likely so we assume the risk.
    withRuntimeLibdir :: FilePath -> IO a -> IO a
    withRuntimeLibdir libdir = bracket_ (setEnv key libdir) (unsetEnv key)
        where key = "GHC_EXACTPRINT_GHC_LIBDIR"
#endif
