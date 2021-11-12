{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Ide.Plugin.AlternateNumberFormat where

import           Control.Lens                         ((^.))
import           Control.Monad.Except                 (ExceptT, MonadIO, liftIO)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.Maybe                           (fromMaybe, isJust)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Development.IDE                      (GetParsedModule (GetParsedModule),
                                                       IdeState, RuleResult,
                                                       Rules,
                                                       ShowDiagnostic (HideDiag),
                                                       define, ideLogger,
                                                       isInsideSrcSpan, noRange,
                                                       runAction,
                                                       srcSpanToRange, use,
                                                       useWithStale)
import           Development.IDE.Core.PositionMapping (PositionMapping)
import           Development.IDE.GHC.Compat           hiding (getSrcSpan)
import           Development.IDE.GHC.Compat.Util      (toList)
import           Development.IDE.Graph.Classes        (Hashable, NFData)
import           Development.IDE.Types.Logger         as Logger
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Conversion                (FormatType,
                                                       alternateFormat,
                                                       toFormatTypes)
import           Ide.Plugin.Literals                  (Literal (..),
                                                       collectLiterals,
                                                       getSrcSpan, getSrcText)
import           Ide.Plugin.Retrie                    (handleMaybe,
                                                       handleMaybeM, response)
import           Ide.Types
import           Language.LSP.Types
import           Language.LSP.Types.Lens              (uri)
import           Prelude                              hiding (log)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionHandler
    , pluginRules = collectLiteralsRule
    }

data CollectLiterals = CollectLiterals
                     deriving (Show, Eq, Generic)

instance Hashable CollectLiterals
instance NFData CollectLiterals

type instance RuleResult CollectLiterals = CollectLiteralsResult

data CollectLiteralsResult = CLR {
    literals      :: [Literal]
    , formatTypes :: [FormatType]
    } deriving (Generic)

instance Show CollectLiteralsResult where
    show _ = "<CollectLiteralResult>"

instance NFData CollectLiteralsResult

collectLiteralsRule :: Rules ()
collectLiteralsRule = define $ \CollectLiterals nfp -> do
    pm <- use GetParsedModule nfp
    -- get the current extensions active and transform them into FormatTypes
    let fmts = getFormatTypes <$> pm
        -- collect all the literals for a file
        lits = collectLiterals . pm_parsed_source <$> pm
        -- make diagnostics for each literal (they will be hidden from editor)
        litDiags = maybe [] (map (mkFileDiagnostic nfp . mkDiagnostic)) lits
    pure (litDiags, CLR <$> lits <*> fmts)
    where
        getFormatTypes = toFormatTypes . toList . extensionFlags . ms_hspp_opts . pm_mod_summary
        mkFileDiagnostic nfp' diag = (nfp', HideDiag, diag)

codeActionHandler :: PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionHandler state _ (CodeActionParams _ _ docId currRange _) = response $ do
    nfp <- getNormalizedFilePath docId
    (CLR{..}, _) <- getCollectLiterals state nfp
        -- remove any invalid literals (see validTarget comment)
    let litsInRange = filter validTarget literals
        -- generate alternateFormats and zip with the literal that generated the alternates
        literalPairs = map (\lit -> (lit, alternateFormat formatTypes lit)) litsInRange
        -- make a code action for every literal and its' alternates (then flatten the result)
        actions = concatMap (\(lit, alts) -> map (mkCodeAction nfp lit) alts) literalPairs

    pure $ List actions
    where
        getSrcTextDefault = fromMaybe "" . getSrcText
        -- for now we ignore literals with no attached source text/span (TH I believe)
        validTarget :: Literal -> Bool
        validTarget lit = let srcSpan = getSrcSpan lit
                              in currRange `contains` srcSpan
                                 && isJust (getSrcText lit)
                                 && isRealSrcSpan srcSpan

        mkCodeAction :: NormalizedFilePath -> Literal -> Text -> Command |? CodeAction
        mkCodeAction nfp lit alt = InR CodeAction {
            _title = "Convert " <> getSrcTextDefault lit <> " into " <> alt
            -- what should this actually be?
            , _kind = Just $ CodeActionUnknown "alternate.style"
            , _diagnostics = Nothing
            , _isPreferred = Just True
            , _disabled = Nothing
            , _edit = Just $ mkWorkspaceEdit nfp lit alt
            , _command = Nothing
            , _xdata = Nothing
            }

        mkWorkspaceEdit :: NormalizedFilePath -> Literal -> Text -> WorkspaceEdit
        mkWorkspaceEdit nfp lit alt = WorkspaceEdit changes Nothing Nothing
            where
                -- NOTE: currently our logic filters our any noRange possibilities
                txtEdit = TextEdit (fromMaybe noRange $ srcSpanToRange $ getSrcSpan lit) alt
                changes = Just $ HashMap.fromList [( filePathToUri $ fromNormalizedFilePath nfp, List [txtEdit])]

-- from HaddockComments.hs
contains :: Range -> SrcSpan -> Bool
contains Range {_start, _end} x = isInsideSrcSpan _start x || isInsideSrcSpan _end x

-- a source span provides no meaningful information to edit
isRealSrcSpan :: SrcSpan -> Bool
isRealSrcSpan (UnhelpfulSpan _) = False
isRealSrcSpan _                 = True

mkDiagnostic :: Literal -> Diagnostic
mkDiagnostic lit = Diagnostic {
            _range = fromMaybe noRange $ srcSpanToRange $ getSrcSpan lit
            , _severity = Just DsHint
            , _code = Nothing
            , _source = getSrcText lit
            , _message = "alternateNumberFormat"
            , _tags = Nothing
            , _relatedInformation = Nothing
            }

getNormalizedFilePath :: Monad m => TextDocumentIdentifier -> ExceptT String m NormalizedFilePath
getNormalizedFilePath docId = handleMaybe "Error: converting to NormalizedFilePath"
        $ uriToNormalizedFilePath
        $ toNormalizedUri (docId ^. uri)

getCollectLiterals :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m (CollectLiteralsResult, PositionMapping)
getCollectLiterals state = handleMaybeM "Error: Could not get ParsedModule"
                . liftIO
                . runAction "AlternateNumberFormat.CollectLiterals" state
                . useWithStale CollectLiterals


logIO :: (MonadIO m, Show a) => IdeState -> a -> m ()
logIO state = liftIO . log state

log :: Show a => IdeState -> a -> IO ()
log state = log' state . show

logIO' :: MonadIO m => IdeState -> String -> m ()
logIO' state = liftIO . log' state

log' :: IdeState -> String -> IO ()
log' state = Logger.logError (ideLogger state) . T.pack

