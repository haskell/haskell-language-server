{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Ide.Plugin.AlternateNumberFormat (descriptor, Log(..)) where

import           Control.Lens                    ((^.))
import           Control.Monad.Except            (ExceptT, MonadIO, liftIO)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE                 (GetParsedModule (GetParsedModule),
                                                  IdeState, RuleResult, Rules,
                                                  define, ideLogger,
                                                  realSrcSpanToRange, runAction,
                                                  use)
import qualified Development.IDE.Core.Shake      as Shake
import           Development.IDE.GHC.Compat      hiding (getSrcSpan)
import           Development.IDE.GHC.Compat.Util (toList)
import           Development.IDE.Graph.Classes   (Hashable, NFData)
import           Development.IDE.Types.Logger    as Logger
import           GHC.Generics                    (Generic)
import           Ide.Plugin.Conversion           (FormatType, alternateFormat,
                                                  toFormatTypes)
import           Ide.Plugin.Literals             (Literal (..), collectLiterals,
                                                  getSrcSpan, getSrcText)
import           Ide.PluginUtils                 (handleMaybe, handleMaybeM,
                                                  response)
import           Ide.Types
import           Language.LSP.Types
import           Language.LSP.Types.Lens         (uri)

newtype Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionHandler
    , pluginRules = collectLiteralsRule recorder
    }

data CollectLiterals = CollectLiterals
                     deriving (Show, Eq, Generic)

instance Hashable CollectLiterals
instance NFData CollectLiterals

type instance RuleResult CollectLiterals = CollectLiteralsResult

data CollectLiteralsResult = CLR
    { literals    :: [Literal]
    , formatTypes :: [FormatType]
    } deriving (Generic)

instance Show CollectLiteralsResult where
    show _ = "<CollectLiteralResult>"

instance NFData CollectLiteralsResult

collectLiteralsRule :: Recorder (WithPriority Log) -> Rules ()
collectLiteralsRule recorder = define (cmapWithPrio LogShake recorder) $ \CollectLiterals nfp -> do
    pm <- use GetParsedModule nfp
    -- get the current extensions active and transform them into FormatTypes
    let fmts = getFormatTypes <$> pm
        -- collect all the literals for a file
        lits = collectLiterals . pm_parsed_source <$> pm
    pure ([], CLR <$> lits <*> fmts)
    where
        getFormatTypes = toFormatTypes . toList . extensionFlags . ms_hspp_opts . pm_mod_summary

codeActionHandler :: PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionHandler state _ (CodeActionParams _ _ docId currRange _) = response $ do
    nfp <- getNormalizedFilePath docId
    CLR{..} <- requestLiterals state nfp
        -- remove any invalid literals (see validTarget comment)
    let litsInRange = filter inCurrentRange literals
        -- generate alternateFormats and zip with the literal that generated the alternates
        literalPairs = map (\lit -> (lit, alternateFormat formatTypes lit)) litsInRange
        -- make a code action for every literal and its' alternates (then flatten the result)
        actions = concatMap (\(lit, alts) -> map (mkCodeAction nfp lit) alts) literalPairs

    logIO state $ "Literals: " <> show literals

    pure $ List actions
    where
        inCurrentRange :: Literal -> Bool
        inCurrentRange lit = let srcSpan = getSrcSpan lit
                              in currRange `contains` srcSpan

        mkCodeAction :: NormalizedFilePath -> Literal -> Text -> Command |? CodeAction
        mkCodeAction nfp lit alt = InR CodeAction {
            _title = "Convert " <> getSrcText lit <> " into " <> alt
            , _kind = Just $ CodeActionUnknown "quickfix.literals.style"
            , _diagnostics = Nothing
            , _isPreferred = Nothing
            , _disabled = Nothing
            , _edit = Just $ mkWorkspaceEdit nfp lit alt
            , _command = Nothing
            , _xdata = Nothing
            }

        mkWorkspaceEdit :: NormalizedFilePath -> Literal -> Text -> WorkspaceEdit
        mkWorkspaceEdit nfp lit alt = WorkspaceEdit changes Nothing Nothing
            where
                txtEdit = TextEdit (realSrcSpanToRange $ getSrcSpan lit) alt
                changes = Just $ HashMap.fromList [( filePathToUri $ fromNormalizedFilePath nfp, List [txtEdit])]

-- from HaddockComments.hs
contains :: Range -> RealSrcSpan -> Bool
contains Range {_start, _end} x = isInsideRealSrcSpan _start x || isInsideRealSrcSpan _end x

isInsideRealSrcSpan :: Position -> RealSrcSpan -> Bool
p `isInsideRealSrcSpan` r = let (Range sp ep) = realSrcSpanToRange r in sp <= p && p <= ep

getNormalizedFilePath :: Monad m => TextDocumentIdentifier -> ExceptT String m NormalizedFilePath
getNormalizedFilePath docId = handleMaybe "Error: converting to NormalizedFilePath"
        $ uriToNormalizedFilePath
        $ toNormalizedUri (docId ^. uri)

requestLiterals :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m CollectLiteralsResult
requestLiterals state = handleMaybeM "Error: Could not Collect Literals"
                . liftIO
                . runAction "AlternateNumberFormat.CollectLiterals" state
                . use CollectLiterals


logIO :: (MonadIO m, Show a) => IdeState -> a -> m ()
logIO state = liftIO . Logger.logDebug (ideLogger state) . T.pack . show

