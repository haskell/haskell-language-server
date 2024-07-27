{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.GADT (descriptor) where

import           Control.Lens                     ((^.))

import           Control.Monad.Error.Class        (MonadError (throwError),
                                                   liftEither)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.Except       (ExceptT, withExceptT)
import           Data.Aeson                       (FromJSON, ToJSON, toJSON)
import           Data.Either.Extra                (maybeToEither)
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           Development.IDE
import           Development.IDE.GHC.Compat

import           Data.Maybe                       (mapMaybe)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Spans.Pragmas    (getFirstPragma,
                                                   insertNewPragma)
import           GHC.Generics                     (Generic)
import           Ide.Plugin.Error
import           Ide.Plugin.GHC
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId "Provides a code action to convert datatypes to GADT syntax")
    { Ide.Types.pluginHandlers =
        mkPluginHandler SMethod_TextDocumentCodeAction codeActionHandler
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
toGADTCommand pId@(PluginId pId') state _ ToGADTParams{..} = withExceptT handleGhcidePluginError $ do
    nfp <- withExceptT GhcidePluginErrors $ getNormalizedFilePathE uri
    (decls, exts) <- getInRangeH98DeclsAndExts state range nfp
    (L ann decl) <- case decls of
        [d] -> pure d
        _   -> throwError $ UnexpectedNumberOfDeclarations (Prelude.length decls)
    deps <- withExceptT GhcidePluginErrors
        $ runActionE (T.unpack pId' <> ".GhcSessionDeps") state
        $ useE GhcSessionDeps nfp
    (hsc_dflags . hscEnv -> df) <- pure deps
    txt <- withExceptT (PrettyGadtError . T.pack) $ liftEither $ T.pack <$> (prettyGADTDecl df . h98ToGADTDecl) decl
    range <- liftEither
        $ maybeToEither FailedToFindDataDeclRange
        $ srcSpanToRange $ locA ann
    pragma <- withExceptT GhcidePluginErrors $ getFirstPragma pId state nfp
    let insertEdit = [insertNewPragma pragma GADTs | all (`notElem` exts) [GADTSyntax, GADTs]]

    _ <- lift $ pluginSendRequest
            SMethod_WorkspaceApplyEdit
            (ApplyWorkspaceEditParams Nothing (workSpaceEdit nfp (TextEdit range txt : insertEdit)))
            (\_ -> pure ())

    pure $ InR Null
    where
        workSpaceEdit nfp edits = WorkspaceEdit
            (pure $ Map.fromList
                [(filePathToUri $ fromNormalizedFilePath nfp,
                edits)])
                 Nothing Nothing

codeActionHandler :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeActionHandler state plId (CodeActionParams _ _ doc range _) = withExceptT handleGhcidePluginError $ do
    nfp <- withExceptT GhcidePluginErrors $ getNormalizedFilePathE (doc ^. L.uri)
    (inRangeH98Decls, _) <- getInRangeH98DeclsAndExts state range nfp
    let actions = map (mkAction . printOutputable . tcdLName . unLoc) inRangeH98Decls
    pure $ InL actions
    where
        mkAction :: T.Text -> Command |? CodeAction
        mkAction name = InR CodeAction{..}
            where
                _title = "Convert \"" <> name <> "\" to GADT syntax"
                _kind = Just CodeActionKind_RefactorRewrite
                _diagnostics = Nothing
                _isPreferred = Nothing
                _disabled = Nothing
                _edit = Nothing
                _command = Just
                    $ mkLspCommand plId toGADTSyntaxCommandId _title (Just [toJSON mkParam])
                _data_ = Nothing

        mkParam = ToGADTParams (doc ^. L.uri) range

-- | Get all H98 decls in the given range, and enabled extensions
getInRangeH98DeclsAndExts :: (MonadIO m) =>
    IdeState
    -> Range
    -> NormalizedFilePath
    -> ExceptT GadtPluginError m ([LTyClDecl GP], [Extension])
getInRangeH98DeclsAndExts state range nfp = do
    pm <- withExceptT GhcidePluginErrors
        $ runActionE "GADT.GetParsedModuleWithComments" state
        $ useE GetParsedModuleWithComments nfp
    let (L _ hsDecls) = hsmodDecls <$> pm_parsed_source pm
        decls = filter isH98DataDecl
            $ mapMaybe getDataDecl
            $ filter (inRange range) hsDecls
        exts = getExtensions pm
    pure (decls, exts)

data GadtPluginError
    = UnexpectedNumberOfDeclarations Int
    | FailedToFindDataDeclRange
    | PrettyGadtError T.Text
    | GhcidePluginErrors PluginError

handleGhcidePluginError ::
    GadtPluginError ->
    PluginError
handleGhcidePluginError = \case
    UnexpectedNumberOfDeclarations nums -> do
        PluginInternalError $ "Expected one declaration but found: " <> T.pack (show nums)
    FailedToFindDataDeclRange ->
        PluginInternalError "Unable to get data decl range"
    PrettyGadtError errMsg ->
        PluginInternalError errMsg
    GhcidePluginErrors errors ->
        errors
