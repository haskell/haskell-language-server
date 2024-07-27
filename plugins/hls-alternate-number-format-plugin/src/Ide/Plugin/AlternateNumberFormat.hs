{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Ide.Plugin.AlternateNumberFormat (descriptor, Log(..)) where

import           Control.Lens                     ((^.))
import           Control.Monad.Except             (ExceptT)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.Map                         as Map
import           Data.Text                        (Text, unpack)
import qualified Data.Text                        as T
import           Development.IDE                  (GetParsedModule (GetParsedModule),
                                                   IdeState, RuleResult, Rules,
                                                   define, realSrcSpanToRange,
                                                   use)
import           Development.IDE.Core.PluginUtils
import qualified Development.IDE.Core.Shake       as Shake
import           Development.IDE.GHC.Compat       hiding (getSrcSpan)
import           Development.IDE.GHC.Util         (getExtensions)
import           Development.IDE.Graph.Classes    (Hashable, NFData, rnf)
import           Development.IDE.Spans.Pragmas    (NextPragmaInfo,
                                                   getFirstPragma,
                                                   insertNewPragma)
import           GHC.Generics                     (Generic)
import           Ide.Logger                       as Logger
import           Ide.Plugin.Conversion            (AlternateFormat,
                                                   ExtensionNeeded (NeedsExtension, NoExtension),
                                                   alternateFormat)
import           Ide.Plugin.Error
import           Ide.Plugin.Literals
import           Ide.Plugin.RangeMap              (RangeMap)
import qualified Ide.Plugin.RangeMap              as RangeMap
import           Ide.Types
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

newtype Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake msg -> pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pId = (defaultPluginDescriptor pId "Provides code actions to convert numeric literals to different formats")
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction codeActionHandler
    , pluginRules = collectLiteralsRule recorder
    }

data CollectLiterals = CollectLiterals
                     deriving (Show, Eq, Generic)

instance Hashable CollectLiterals
instance NFData CollectLiterals

type instance RuleResult CollectLiterals = CollectLiteralsResult

data CollectLiteralsResult = CLR
    { literals          :: RangeMap Literal
    , enabledExtensions :: [GhcExtension]
    } deriving (Generic)

newtype GhcExtension = GhcExtension { unExt :: Extension }

instance NFData GhcExtension where
    rnf x = x `seq` ()

instance Show CollectLiteralsResult where
    show _ = "<CollectLiteralResult>"

instance NFData CollectLiteralsResult

collectLiteralsRule :: Recorder (WithPriority Log) -> Rules ()
collectLiteralsRule recorder = define (cmapWithPrio LogShake recorder) $ \CollectLiterals nfp -> do
    pm <- use GetParsedModule nfp
    -- get the current extensions active and transform them into FormatTypes
    let exts = map GhcExtension . getExtensions <$> pm
        -- collect all the literals for a file
        lits = collectLiterals . pm_parsed_source <$> pm
        litMap = RangeMap.fromList (realSrcSpanToRange . getSrcSpan) <$> lits
    pure ([], CLR <$> litMap <*> exts)

codeActionHandler :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeActionHandler state pId (CodeActionParams _ _ docId currRange _) = do
    nfp <- getNormalizedFilePathE (docId ^. L.uri)
    CLR{..} <- requestLiterals pId state nfp
    pragma <- getFirstPragma pId state nfp
        -- remove any invalid literals (see validTarget comment)
    let litsInRange = RangeMap.filterByRange currRange literals
        -- generate alternateFormats and zip with the literal that generated the alternates
        literalPairs = map (\lit -> (lit, alternateFormat lit)) litsInRange
        -- make a code action for every literal and its' alternates (then flatten the result)
        actions = concatMap (\(lit, alts) -> map (mkCodeAction nfp lit enabledExtensions pragma) alts) literalPairs
    pure $ InL actions
    where
        mkCodeAction :: NormalizedFilePath -> Literal -> [GhcExtension] -> NextPragmaInfo -> AlternateFormat -> Command |? CodeAction
        mkCodeAction nfp lit enabled npi af@(alt, ext) = InR CodeAction {
            _title = mkCodeActionTitle lit af enabled
            , _kind = Just $ CodeActionKind_Custom "quickfix.literals.style"
            , _diagnostics = Nothing
            , _isPreferred = Nothing
            , _disabled = Nothing
            , _edit = Just $ mkWorkspaceEdit nfp edits
            , _command = Nothing
            , _data_ = Nothing
            }
            where
                edits =  [TextEdit (realSrcSpanToRange $ getSrcSpan lit) alt] <> pragmaEdit
                pragmaEdit = case ext of
                    NeedsExtension ext' -> [insertNewPragma npi ext' | needsExtension ext' enabled]
                    NoExtension         -> []

        mkWorkspaceEdit :: NormalizedFilePath -> [TextEdit] -> WorkspaceEdit
        mkWorkspaceEdit nfp edits = WorkspaceEdit changes Nothing Nothing
            where
                changes = Just $ Map.singleton (filePathToUri $ fromNormalizedFilePath nfp) edits

mkCodeActionTitle :: Literal -> AlternateFormat -> [GhcExtension] -> Text
mkCodeActionTitle lit (alt, ext) ghcExts
    | (NeedsExtension ext') <- ext
    , needsExtension ext' ghcExts = title <> " (needs extension: " <> T.pack (show ext') <> ")"
    | otherwise = title
    where
        title = "Convert " <> getSrcText lit <> " into " <> alt


-- | Checks whether the extension given is already enabled
needsExtension :: Extension -> [GhcExtension] -> Bool
needsExtension ext ghcExts = ext `notElem` map unExt ghcExts

requestLiterals :: MonadIO m => PluginId -> IdeState -> NormalizedFilePath -> ExceptT PluginError m CollectLiteralsResult
requestLiterals (PluginId pId) state =
    runActionE (unpack pId <> ".CollectLiterals") state
    . useE CollectLiterals
