{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.Hlint
  (
    descriptor
  --, provider
  ) where
import Refact.Apply
import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Exception
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson.Types (ToJSON(..), FromJSON(..), Value(..))
import Data.Binary
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import Development.IDE.Types.Diagnostics as D
import Development.IDE.Types.Location
import Development.Shake
-- import Development.Shake hiding ( Diagnostic )
import GHC hiding (DynFlags(..))

#ifdef GHC_LIB
import Development.IDE.Core.RuleTypes (GhcSession(..))
import Development.IDE.GHC.Util (hscEnv)
import RealGHC (DynFlags(..))
import RealGHC.HscTypes (hsc_dflags)
import qualified RealGHC.EnumSet as EnumSet
import Language.Haskell.GhclibParserEx.GHC.Driver.Session as GhclibParserEx (readExtension)
#endif

import Ide.Logger
import Ide.Types
import Ide.Plugin
import Ide.Plugin.Config
import Ide.PluginUtils
import Language.Haskell.HLint as Hlint
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types      as LSP
import qualified Language.Haskell.LSP.Types.Lens as LSP
import Text.Regex.TDFA.Text()
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
  { pluginRules = rules
  , pluginCommands =
      [ PluginCommand "applyOne" "Apply a single hint" applyOneCmd
      , PluginCommand "applyAll" "Apply all hints to the file" applyAllCmd
      ]
    , pluginCodeActionProvider = Just codeActionProvider
  }

data GetHlintDiagnostics = GetHlintDiagnostics
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHlintDiagnostics
instance NFData   GetHlintDiagnostics
instance Binary   GetHlintDiagnostics

type instance RuleResult GetHlintDiagnostics = ()

rules :: Rules ()
rules = do
  define $ \GetHlintDiagnostics file -> do
    hlintOn' <- hlintOn <$> getClientConfigAction
    logm $ "hlint:rules:hlintOn=" <> show hlintOn'
    ideas <- if hlintOn' then getIdeas file else return (Right [])
    return (diagnostics file ideas, Just ())

  getHlintSettingsRule (HlintEnabled [])

  action $ do
    files <- getFilesOfInterest
    void $ uses GetHlintDiagnostics $ HashSet.toList files

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
          , _severity = Just LSP.DsInfo
          , _code     = Just (LSP.StringValue $ T.pack $ ideaHint idea)
          , _source   = Just "hlint"
          , _message  = T.pack $ show idea
          , _relatedInformation = Nothing
          , _tags     = Nothing
        }

      parseErrorToDiagnostic :: ParseError -> Diagnostic
      parseErrorToDiagnostic (Hlint.ParseError l msg contents) =
        LSP.Diagnostic {
            _range    = srcSpanToRange l
          , _severity = Just LSP.DsInfo
          , _code     = Just (LSP.StringValue "parser")
          , _source   = Just "hlint"
          , _message  = T.unlines [T.pack msg,T.pack contents]
          , _relatedInformation = Nothing
          , _tags     = Nothing
        }
      -- This one is defined in Development.IDE.GHC.Error but here
      -- the types could come from ghc-lib or ghc
      srcSpanToRange :: SrcSpan -> LSP.Range
      srcSpanToRange (RealSrcSpan span) = Range {
          _start = LSP.Position {
                _line = srcSpanStartLine span - 1
              , _character  = srcSpanStartCol span - 1}
        , _end   = LSP.Position {
                _line = srcSpanEndLine span - 1
             , _character = srcSpanEndCol span - 1}
        }
      srcSpanToRange (UnhelpfulSpan _) = noRange

getIdeas :: NormalizedFilePath -> Action (Either ParseError [Idea])
getIdeas nfp = do
  logm $ "hlint:getIdeas:file:" ++ show nfp
  (flags, classify, hint) <- useNoFile_ GetHlintSettings

  let applyHints' (Just (Right modEx)) = Right $ applyHints classify hint [modEx]
      applyHints' (Just (Left err)) = Left err
      applyHints' Nothing = Right []

  fmap applyHints' (moduleEx flags)

  where moduleEx :: ParseFlags -> Action (Maybe (Either ParseError ModuleEx))
#ifdef GHC_LIB
        moduleEx flags = do
          flags' <- setExtensions flags
          (_, contents) <- getFileContents nfp
          let fp = fromNormalizedFilePath nfp
          let contents' = T.unpack <$> contents
          Just <$> (liftIO $ parseModuleEx flags' fp contents')

        setExtensions flags = do
          hsc <- hscEnv <$> use_ GhcSession nfp
          let dflags = hsc_dflags hsc
          let hscExts = EnumSet.toList (extensionFlags dflags)
          let hlintExts = mapMaybe (GhclibParserEx.readExtension . show) hscExts
          logm $ "hlint:getIdeas:setExtensions:" ++ show hlintExts
          return $ flags { enabledExtensions = hlintExts }
#else
        moduleEx _flags = do
          mbpm <- getParsedModule nfp
          return $ createModule <$> mbpm
          where createModule pm = Right (createModuleEx anns modu)
                  where anns = pm_annotations pm
                        modu = pm_parsed_source pm
#endif

-- ---------------------------------------------------------------------

data HlintUsage
  = HlintEnabled { cmdArgs :: [String] }
  | HlintDisabled
  deriving Show

data GetHlintSettings = GetHlintSettings
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHlintSettings
instance NFData   GetHlintSettings
instance NFData Hint where rnf = rwhnf
instance NFData Classify where rnf = rwhnf
instance NFData ParseFlags where rnf = rwhnf
instance Show Hint where show = const "<hint>"
instance Show ParseFlags where show = const "<parseFlags>"
instance Binary GetHlintSettings

type instance RuleResult GetHlintSettings = (ParseFlags, [Classify], Hint)

getHlintSettingsRule :: HlintUsage -> Rules ()
getHlintSettingsRule usage =
    defineNoFile $ \GetHlintSettings ->
      liftIO $ case usage of
          HlintEnabled cmdArgs -> argsSettings cmdArgs
          HlintDisabled -> fail "hlint configuration unspecified"

-- ---------------------------------------------------------------------

codeActionProvider :: CodeActionProvider
codeActionProvider _lf ideState plId docId _ context = Right . LSP.List . map CACodeAction <$> getCodeActions
  where

    getCodeActions = do
        applyOne <- applyOneActions
        diags <- getDiagnostics ideState
        let docNfp = toNormalizedFilePath' <$> uriToFilePath' (docId ^. LSP.uri)
            numHintsInDoc = length
              [d | (nfp, _, d) <- diags
                 , d ^. LSP.source == Just "hlint"
                 , Just nfp == docNfp
              ]
        -- We only want to show the applyAll code action if there is more than 1
        -- hint in the current document
        if numHintsInDoc >= 2 then do
          applyAll <- applyAllAction
          pure $ applyAll:applyOne
        else
          pure applyOne

    applyAllAction = do
      let args = Just [toJSON (docId ^. LSP.uri)]
      cmd <- mkLspCommand plId "applyAll" "Apply all hints" args
      pure $ LSP.CodeAction "Apply all hints" (Just LSP.CodeActionQuickFix) Nothing Nothing (Just cmd)

    applyOneActions :: IO [LSP.CodeAction]
    applyOneActions = catMaybes <$> mapM mkHlintAction (filter validCommand diags)

    -- |Some hints do not have an associated refactoring
    validCommand (LSP.Diagnostic _ _ (Just (LSP.StringValue code)) (Just "hlint") _ _ _) =
      code /= "Eta reduce"
    validCommand _ = False

    LSP.List diags = context ^. LSP.diagnostics

    mkHlintAction :: LSP.Diagnostic -> IO (Maybe LSP.CodeAction)
    mkHlintAction diag@(LSP.Diagnostic (LSP.Range start _) _s (Just (LSP.StringValue code)) (Just "hlint") m _ _) =
      Just . codeAction <$> mkLspCommand plId "applyOne" title (Just args)
     where
       codeAction cmd = LSP.CodeAction title (Just LSP.CodeActionQuickFix) (Just (LSP.List [diag])) Nothing (Just cmd)
       title = "Apply hint:" <> head (T.lines m)
       -- need 'file', 'start_pos' and hint title (to distinguish between alternative suggestions at the same location)
       args = [toJSON (AOP (docId ^. LSP.uri) start code)]
    mkHlintAction (LSP.Diagnostic _r _s _c _source _m _ _) = return Nothing

-- ---------------------------------------------------------------------

applyAllCmd :: CommandFunction Uri
applyAllCmd lf ide uri = do
  let file = maybe (error $ show uri ++ " is not a file.")
                    toNormalizedFilePath'
                   (uriToFilePath' uri)
  -- Persist the virtual file first since apply-refact works on files, not text content
  mvfp <- persistVirtualFileFunc lf (toNormalizedUri uri)
  case mvfp of
    Nothing -> pure (Left (responseError (T.pack $ "Couldn't persist virtual file for " ++ show uri)), Nothing)
    Just vfp -> do
      logm $ "hlint:applyAllCmd:file=" ++ show file
      res <- applyHint ide (toNormalizedFilePath vfp) file Nothing
      logm $ "hlint:applyAllCmd:res=" ++ show res
      return $
        case res of
          Left err -> (Left (responseError (T.pack $ "hlint:applyAll: " ++ show err)), Nothing)
          Right fs -> (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams fs))

-- ---------------------------------------------------------------------

data ApplyOneParams = AOP
  { file      :: Uri
  , start_pos :: Position
  -- | There can be more than one hint suggested at the same position, so HintTitle is used to distinguish between them.
  , hintTitle :: HintTitle
  } deriving (Eq,Show,Generic,FromJSON,ToJSON)

type HintTitle = T.Text

data OneHint = OneHint
  { oneHintPos :: Position
  , oneHintTitle :: HintTitle
  } deriving (Eq, Show)

applyOneCmd :: CommandFunction ApplyOneParams
applyOneCmd lf ide (AOP uri pos title) = do
  let oneHint = OneHint pos title
  let file = maybe (error $ show uri ++ " is not a file.") toNormalizedFilePath'
                   (uriToFilePath' uri)
  -- Persist the virtual file first since apply-refact works on files, not text content
  mvfp <- persistVirtualFileFunc lf (toNormalizedUri uri)
  case mvfp of
    Nothing -> pure (Left (responseError (T.pack $ "Couldn't persist virtual file for " ++ show uri)), Nothing)
    Just vfp -> do
      res <- applyHint ide (toNormalizedFilePath vfp) file (Just oneHint)
      logm $ "hlint:applyOneCmd:file=" ++ show file
      logm $ "hlint:applyOneCmd:res=" ++ show res
      return $
        case res of
          Left err -> (Left (responseError (T.pack $ "hlint:applyOne: " ++ show err)), Nothing)
          Right fs -> (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams fs))

applyHint :: IdeState -> NormalizedFilePath -> NormalizedFilePath -> Maybe OneHint -> IO (Either String WorkspaceEdit)
applyHint ide virtualFp actualFp mhint =
  runExceptT $ do
    ideas <- bimapExceptT showParseError id $ ExceptT $ liftIO $ runAction "applyHint" ide $ getIdeas virtualFp
    let ideas' = maybe ideas (`filterIdeas` ideas) mhint
    let commands = map (show &&& ideaRefactoring) ideas'
    liftIO $ logm $ "applyHint:apply=" ++ show commands
    -- set Nothing as "position" for "applyRefactorings" because
    -- applyRefactorings expects the provided position to be _within_ the scope
    -- of each refactoring it will apply.
    -- But "Idea"s returned by HLint point to starting position of the expressions
    -- that contain refactorings, so they are often outside the refactorings' boundaries.
    -- Example:
    -- Given an expression "hlintTest = reid $ (myid ())"
    -- Hlint returns an idea at the position (1,13)
    -- That contains "Redundant brackets" refactoring at position (1,20):
    --
    -- [("src/App/Test.hs:5:13: Warning: Redundant bracket\nFound:\n  reid $ (myid ())\nWhy not:\n  reid $ myid ()\n",[Replace {rtype = Expr, pos = SrcSpan {startLine = 5, startCol = 20, endLine = 5, endCol = 29}, subts = [("x",SrcSpan {startLine = 5, startCol = 21, endLine = 5, endCol = 28})], orig = "x"}])]
    --
    -- If we provide "applyRefactorings" with "Just (1,13)" then
    -- the "Redundant bracket" hint will never be executed
    -- because SrcSpan (1,20,??,??) doesn't contain position (1,13).
    let fp = fromNormalizedFilePath virtualFp
    res <- liftIO $ (Right <$> applyRefactorings Nothing commands fp) `catches`
              [ Handler $ \e -> return (Left (show (e :: IOException)))
              , Handler $ \e -> return (Left (show (e :: ErrorCall)))
              ]
    case res of
      Right appliedFile -> do
        let uri = fromNormalizedUri (filePathToUri' actualFp)
        (_, mbOldContent) <- liftIO $ runAction "hlint" ide $ getFileContents actualFp
        oldContent <- maybe (liftIO $ T.readFile fp) return mbOldContent
        let wsEdit = diffText' True (uri, oldContent) (T.pack appliedFile) IncludeDeletions
        liftIO $ logm $ "hlint:applyHint:diff=" ++ show wsEdit
        ExceptT $ Right <$> (return wsEdit)
      Left err ->
        throwE (show err)
    where
          -- | If we are only interested in applying a particular hint then
          -- let's filter out all the irrelevant ideas
          filterIdeas :: OneHint -> [Idea] -> [Idea]
          filterIdeas (OneHint (Position l c) title) ideas =
            let title' = T.unpack title
                ideaPos = (srcSpanStartLine &&& srcSpanStartCol) . toRealSrcSpan . ideaSpan
            in filter (\i -> ideaHint i == title' && ideaPos i == (l+1, c+1)) ideas

          toRealSrcSpan (RealSrcSpan real) = real
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
