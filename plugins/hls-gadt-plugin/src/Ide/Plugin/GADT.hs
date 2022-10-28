{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.GADT (descriptor) where

import           Control.Lens                  ((^.))
import           Control.Monad.Except
import           Data.Aeson                    (FromJSON, ToJSON, Value (Null),
                                                toJSON)
import           Data.Either.Extra             (maybeToEither)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.Text                     as T
import           Development.IDE
import           Development.IDE.GHC.Compat

import           Control.Monad.Trans.Except    (throwE)
import           Data.Maybe                    (mapMaybe)
import           Development.IDE.Spans.Pragmas (getFirstPragma, insertNewPragma)
import           GHC.Generics                  (Generic)
import           Ide.Plugin.GHC
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server           (sendRequest)
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens       as L

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { Ide.Types.pluginHandlers =
        mkPluginHandler STextDocumentCodeAction codeActionHandler
    , pluginCommands =
        [PluginCommand toGADTSyntaxCommandId "convert data decl to GADT syntax" (toGADTCommand plId)]
    }

-- | Parameter used in the command
data ToGADTParams = ToGADTParams
    { uri   :: Uri
    , range :: Range
    } deriving (Generic, ToJSON, FromJSON)

toGADTSyntaxCommandId :: CommandId
toGADTSyntaxCommandId = "GADT.toGADT"

-- | A command replaces H98 data decl with GADT decl in place
toGADTCommand :: PluginId -> CommandFunction IdeState ToGADTParams
toGADTCommand pId@(PluginId pId') state ToGADTParams{..} = pluginResponse $ do
    nfp <- getNormalizedFilePath uri
    (decls, exts) <- getInRangeH98DeclsAndExts state range nfp
    (L ann decl) <- case decls of
        [d] -> pure d
        _   -> throwE $ "Expected 1 declaration, but got " <> show (Prelude.length decls)
    deps <- liftIO $ runAction (T.unpack pId' <> ".GhcSessionDeps") state $ use GhcSessionDeps nfp
    (hsc_dflags . hscEnv -> df) <- liftEither
        $ maybeToEither "Get GhcSessionDeps failed" deps
    txt <- liftEither $ T.pack <$> (prettyGADTDecl df . h98ToGADTDecl) decl
    range <- liftEither
        $ maybeToEither "Unable to get data decl range"
        $ srcSpanToRange $ locA ann
    pragma <- getFirstPragma pId state nfp
    let insertEdit = [insertNewPragma pragma GADTs | all (`notElem` exts) [GADTSyntax, GADTs]]

    _ <- lift $ sendRequest
            SWorkspaceApplyEdit
            (ApplyWorkspaceEditParams Nothing (workSpaceEdit nfp (TextEdit range txt : insertEdit)))
            (\_ -> pure ())

    pure Null
    where
        workSpaceEdit nfp edits = WorkspaceEdit
            (pure $ HashMap.fromList
                [(filePathToUri $ fromNormalizedFilePath nfp,
                 List edits)])
                 Nothing Nothing

codeActionHandler :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionHandler state plId (CodeActionParams _ _ doc range _) = pluginResponse $ do
    nfp <- getNormalizedFilePath (doc ^. L.uri)
    (inRangeH98Decls, _) <- getInRangeH98DeclsAndExts state range nfp
    let actions = map (mkAction . printOutputable . tcdLName . unLoc) inRangeH98Decls
    pure $ List actions
    where
        mkAction :: T.Text -> Command |? CodeAction
        mkAction name = InR CodeAction{..}
            where
                _title = "Convert \"" <> name <> "\" to GADT syntax"
                _kind = Just CodeActionRefactorRewrite
                _diagnostics = Nothing
                _isPreferred = Nothing
                _disabled = Nothing
                _edit = Nothing
                _command = Just
                    $ mkLspCommand plId toGADTSyntaxCommandId _title (Just [toJSON mkParam])
                _xdata = Nothing

        mkParam = ToGADTParams (doc ^. L.uri) range

-- | Get all H98 decls in the given range, and enabled extensions
getInRangeH98DeclsAndExts :: (MonadIO m) =>
    IdeState
    -> Range
    -> NormalizedFilePath
    -> ExceptT String m ([LTyClDecl GP], [Extension])
getInRangeH98DeclsAndExts state range nfp = do
    pm <- handleMaybeM "Unable to get ParsedModuleWithComments"
        $ liftIO
        $ runAction "GADT.GetParsedModuleWithComments" state
        $ use GetParsedModuleWithComments nfp
    let (L _ hsDecls) = hsmodDecls <$> pm_parsed_source pm
        decls = filter isH98DataDecl
            $ mapMaybe getDataDecl
            $ filter (inRange range) hsDecls
        exts = getExtensions pm
    pure (decls, exts)
