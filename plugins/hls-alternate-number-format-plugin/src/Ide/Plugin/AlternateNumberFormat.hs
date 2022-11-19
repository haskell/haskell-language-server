{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Ide.Plugin.AlternateNumberFormat (descriptor, Log(..)) where

import           Control.Lens                  ((^.))
import           Control.Monad.Except          (ExceptT, MonadIO, liftIO)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text                     (Text, unpack)
import qualified Data.Text                     as T
import           Development.IDE               (GetParsedModule (GetParsedModule),
                                                IdeState, RuleResult, Rules,
                                                define, realSrcSpanToRange,
                                                runAction, use)
import qualified Development.IDE.Core.Shake    as Shake
import           Development.IDE.GHC.Compat    hiding (getSrcSpan)
import           Development.IDE.GHC.Util      (getExtensions)
import           Development.IDE.Graph.Classes (Hashable, NFData, rnf)
import           Development.IDE.Spans.Pragmas (NextPragmaInfo, getFirstPragma,
                                                insertNewPragma)
import           Development.IDE.Types.Logger  as Logger
import           GHC.Generics                  (Generic)
import           Ide.Plugin.Conversion         (AlternateFormat,
                                                ExtensionNeeded (NeedsExtension, NoExtension),
                                                alternateFormat)
import           Ide.Plugin.Literals
import           Ide.PluginUtils               (getNormalizedFilePath,
                                                handleMaybeM, pluginResponse)
import           Ide.Types
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens       as L

newtype Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pId = (defaultPluginDescriptor pId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionHandler
    , pluginRules = collectLiteralsRule recorder
    }

data CollectLiterals = CollectLiterals
                     deriving (Show, Eq, Generic)

instance Hashable CollectLiterals
instance NFData CollectLiterals

type instance RuleResult CollectLiterals = CollectLiteralsResult

data CollectLiteralsResult = CLR
    { literals          :: [Literal]
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
    pure ([], CLR <$> lits <*> exts)

codeActionHandler :: PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionHandler state pId (CodeActionParams _ _ docId currRange _) = pluginResponse $ do
    nfp <- getNormalizedFilePath (docId ^. L.uri)
    CLR{..} <- requestLiterals pId state nfp
    pragma <- getFirstPragma pId state nfp
        -- remove any invalid literals (see validTarget comment)
    let litsInRange = filter inCurrentRange literals
        -- generate alternateFormats and zip with the literal that generated the alternates
        literalPairs = map (\lit -> (lit, alternateFormat lit)) litsInRange
        -- make a code action for every literal and its' alternates (then flatten the result)
        actions = concatMap (\(lit, alts) -> map (mkCodeAction nfp lit enabledExtensions pragma) alts) literalPairs
    pure $ List actions
    where
        inCurrentRange :: Literal -> Bool
        inCurrentRange lit = let srcSpan = getSrcSpan lit
                              in currRange `contains` srcSpan

        mkCodeAction :: NormalizedFilePath -> Literal -> [GhcExtension] -> NextPragmaInfo -> AlternateFormat -> Command |? CodeAction
        mkCodeAction nfp lit enabled npi af@(alt, ext) = InR CodeAction {
            _title = mkCodeActionTitle lit af enabled
            , _kind = Just $ CodeActionUnknown "quickfix.literals.style"
            , _diagnostics = Nothing
            , _isPreferred = Nothing
            , _disabled = Nothing
            , _edit = Just $ mkWorkspaceEdit nfp edits
            , _command = Nothing
            , _xdata = Nothing
            }
            where
                edits =  [TextEdit (realSrcSpanToRange $ getSrcSpan lit) alt] <> pragmaEdit
                pragmaEdit = case ext of
                    NeedsExtension ext' -> [insertNewPragma npi ext' | needsExtension ext' enabled]
                    NoExtension         -> []

        mkWorkspaceEdit :: NormalizedFilePath -> [TextEdit] -> WorkspaceEdit
        mkWorkspaceEdit nfp edits = WorkspaceEdit changes Nothing Nothing
            where
                changes = Just $ HashMap.fromList [(filePathToUri $ fromNormalizedFilePath nfp, List edits)]

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

-- from HaddockComments.hs
contains :: Range -> RealSrcSpan -> Bool
contains Range {_start, _end} x = isInsideRealSrcSpan _start x || isInsideRealSrcSpan _end x

isInsideRealSrcSpan :: Position -> RealSrcSpan -> Bool
p `isInsideRealSrcSpan` r = let (Range sp ep) = realSrcSpanToRange r in sp <= p && p <= ep

requestLiterals :: MonadIO m => PluginId -> IdeState -> NormalizedFilePath -> ExceptT String m CollectLiteralsResult
requestLiterals (PluginId pId) state = handleMaybeM "Could not Collect Literals"
                . liftIO
                . runAction (unpack pId <> ".CollectLiterals") state
                . use CollectLiterals
