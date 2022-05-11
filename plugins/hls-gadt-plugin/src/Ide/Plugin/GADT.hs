{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.GADT (descriptor) where

import           Control.Lens               ((^.))
import           Control.Monad.Except
import           Data.Aeson                 (FromJSON, ToJSON, Value (Null),
                                             toJSON)
import           Data.Either.Extra          (maybeToEither)
import qualified Data.HashMap.Lazy          as HashMap
import qualified Data.Text                  as T
import           Development.IDE
import           Development.IDE.GHC.Compat

import           Data.Maybe                 (mapMaybe)
import           GHC.Generics               (Generic)
import           Ide.Plugin.GHC
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server        (sendRequest)
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens    as L

-- TODO: Add GADTs pragma

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { Ide.Types.pluginHandlers =
        mkPluginHandler STextDocumentCodeAction codeActionHandler
    , pluginCommands =
        [PluginCommand toGADTSyntaxCommandId "convert data decl to GADT syntax" toGADTCommand]
    }

-- | Parameter used in the command
data ToGADTParams = ToGADTParams
    { uri   :: Uri
    , range :: Range
    } deriving (Generic, ToJSON, FromJSON)

toGADTSyntaxCommandId :: CommandId
toGADTSyntaxCommandId = "GADT.toGADT"

-- | A command replaces H98 data decl with GADT decl in place
toGADTCommand :: CommandFunction IdeState ToGADTParams
toGADTCommand state ToGADTParams{..} = response $ do
    nfp <- handleMaybe
        ("Unable to convert " <> show uri <> " to NormalizedFilePath")
        $ uriToNormalizedFilePath $ toNormalizedUri uri
    decls <- getInRangeH98Decls state range nfp
    (L ann decl) <- handleMaybe
        ("Expect 1 decl, but got " <> show (Prelude.length decls))
        (if Prelude.length decls == 1 then Just $ head decls else Nothing)
    deps <- liftIO $ runAction "GADT.GhcSessionDeps" state $ use GhcSessionDeps nfp
    (hsc_dflags . hscEnv -> df) <- ExceptT
        $ pure
        $ maybeToEither "Get GhcSessionDeps failed" deps
    txt <- ExceptT $ pure $ T.pack <$> (prettyGADTDecl df . h98ToGADTDecl) decl
    range <- ExceptT
        $ pure
        $ maybeToEither "Unable to get data decl range"
        $ srcSpanToRange $ locA ann
    _ <- lift $ sendRequest
            SWorkspaceApplyEdit
            (ApplyWorkspaceEditParams Nothing (workSpaceEdit nfp txt range))
            (\_ -> pure ())
    pure Null
    where
        workSpaceEdit nfp txt range = WorkspaceEdit
            (pure $ HashMap.fromList
                [(filePathToUri $ fromNormalizedFilePath nfp,
                 List [TextEdit range txt])])
                 Nothing Nothing

codeActionHandler :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionHandler state plId (CodeActionParams _ _ doc range _) = response $ do
    nfp <- getNormalizedFilePath plId doc
    inRangeH98Decls <- getInRangeH98Decls state range nfp
    let actions = map (mkAction . printOutputable . tcdLName . unLoc) inRangeH98Decls
    pure $ List actions
    where
        mkAction :: T.Text -> Command |? CodeAction
        mkAction name = InR CodeAction{..}
            where
                _title = "Convert " <> name <> " to GADT syntax"
                _kind = Just CodeActionRefactorRewrite
                _diagnostics = Nothing
                _isPreferred = Nothing
                _disabled = Nothing
                _edit = Nothing
                _command = Just
                    $ mkLspCommand plId toGADTSyntaxCommandId _title (Just [toJSON mkParam])
                _xdata = Nothing

        mkParam = ToGADTParams (doc ^. L.uri) range

getInRangeH98Decls :: (Monad (t IO), MonadTrans t) =>
    IdeState
    -> Range
    -> NormalizedFilePath
    -> ExceptT String (t IO) [LTyClDecl GP]
getInRangeH98Decls state range nfp = do
    pm <- handleMaybeM "Unable to get ParsedModuleWithComments"
        $ lift
        $ runAction "GADT.GetParsedModuleWithComments" state
        $ use GetParsedModuleWithComments nfp
    let (L _ hsDecls) = hsmodDecls <$> pm_parsed_source pm
    pure $ filter isH98DataDecl $ mapMaybe getDataDecl $ filter (inRange range) hsDecls
