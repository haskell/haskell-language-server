{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.UnderlyingType where

import           Control.Lens                         ((^.))
import           Control.Monad                        (forM)
import           Control.Monad.IO.Class
import           Control.Monad.RWS                    (lift)
import qualified Data.Aeson                           as Aeson
import           Data.Either                          (rights)
import qualified Data.Map                             as M
import           Data.Maybe                           (catMaybes, fromMaybe)
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake           (getShakeExtras)
import           Development.IDE.GHC.Compat.Core      (Name)
import           Development.IDE.Spans.AtPoint        (pointCommand)
import           GHC.Iface.Ext.Types
import           GHC.Iface.Ext.Utils                  (nodeInfo)
import           Ide.Plugin.Error                     (getNormalizedFilePathE)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message        (Method (Method_TextDocumentCodeAction),
                                                       SMethod (SMethod_TextDocumentCodeAction))
import           Language.LSP.Protocol.Types

data Log
  = LogPluginCalled Uri Range
  | LogIdentifiersFound Position [Name]
  | LogTypeLocationsFound [(Location, Identifier)]
  | LogProcessingLocation Location Identifier
  | LogActionCreated Name Name

instance Pretty Log where
  pretty = \case
    LogPluginCalled uri range ->
      "Plugin called for" <+> pretty (show uri) <+> "at range" <+> pretty (show range)
    LogIdentifiersFound pos identifiers ->
      "Found" <+> pretty (length identifiers) <+> "identifiers at" <+> pretty (show pos) <> ":" <+>
        pretty (T.intercalate ", " (map printOutputable identifiers))
    LogTypeLocationsFound locations ->
      "Found" <+> pretty (length locations) <+> "type locations"
    LogProcessingLocation loc identifier ->
      "Processing location" <+> pretty (show loc) <+> "for" <+> pretty (printOutputable identifier)
    LogActionCreated varName typeName ->
      "Created action for" <+> pretty (printOutputable varName) <+> "->" <+> pretty (printOutputable typeName)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId "Generates actions for going to the underlying type's definition.")
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction (provider recorder)
    }

provider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
provider recorder state _ (CodeActionParams _ _ (TextDocumentIdentifier uri) range _) = do
  logWith recorder Debug $ LogPluginCalled uri range

  nfp <- getNormalizedFilePathE uri

  (HAR _ hieAst _ _ hieKind, posMapping) <-
    runActionE "GetHieAst" state $
      useWithStaleE GetHieAst nfp

  let position = fromMaybe (range ^. L.start) $ fromCurrentPosition posMapping (range ^. L.start)
      namesAtPosition = mconcat $ pointCommand hieAst position (extractNamesAtPosition hieKind)

  logWith recorder Debug $ LogIdentifiersFound position namesAtPosition

  actions <- case namesAtPosition of
    [] -> pure []
    identifiers -> do
      typeActions <- forM identifiers $ \bindingName -> do
        locationForIdentifierType <- runActionE "TypeCheck" state $ do
          shakeExtras <- lift getShakeExtras
          result <- liftIO $ runIdeAction "Get Type Definition" shakeExtras $ getTypeDefinition nfp position
          pure $ fromMaybe [] result

        logWith recorder Debug $ LogTypeLocationsFound locationForIdentifierType

        typeDefActions <- forM locationForIdentifierType $ \(loc, identifier) -> do
          logWith recorder Debug $ LogProcessingLocation loc identifier

          case identifier of
            Left _moduleName -> pure Nothing
            Right underlyingTypeName -> do
              logWith recorder Debug $ LogActionCreated bindingName underlyingTypeName
              pure $ Just $ createGoToTypeDefAction bindingName underlyingTypeName loc

        pure $ catMaybes typeDefActions

      pure $ mconcat typeActions

  pure $ InL actions

extractNamesAtPosition :: HieKind a -> HieAST a -> [Name]
extractNamesAtPosition hieKind ast =
  case hieKind of
    HieFresh -> rights $ map fst $ M.toList $ nodeIdentifiers $ nodeInfo ast
    HieFromDisk {} -> []

createGoToTypeDefAction :: Name -> Name -> Location -> (Command |? CodeAction)
createGoToTypeDefAction boundVarName underlyingTypeName loc = do
  let defRange = loc ^. L.range
  InR $
    CodeAction
      ("Go to definition of " <> printOutputable underlyingTypeName <> " (inferred from " <> printOutputable boundVarName <> "'s type)")
      (Just $ CodeActionKind_Custom "GoToUnderlyingTypeDefinition")
      Nothing
      Nothing
      Nothing
      Nothing
      ( Just $
          Command
            "Go to definition"
            -- TODO: How to decouple this from VS code?
            "vscode.open"
            ( Just
                [ Aeson.toJSON $ loc ^. L.uri,
                  Aeson.object
                    [ "selection"
                        Aeson..= Aeson.object
                          [ "start"
                              Aeson..= Aeson.object
                                [ "line" Aeson..= (defRange ^. L.start . L.line),
                                  "character" Aeson..= (defRange ^. L.start . L.character)
                                ],
                            "end"
                              Aeson..= Aeson.object
                                [ "line" Aeson..= (defRange ^. L.end . L.line),
                                  "character" Aeson..= (defRange ^. L.end . L.character)
                                ]
                          ]
                    ]
                ]
            )
      )
      Nothing
