{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Wingman.LanguageServer.SnippetTextEdit where

import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Text as T
import qualified Ide.Plugin.Config
import           Ide.Types
import           Language.LSP.Server (LspM)
import           Language.LSP.Types
import           Wingman.LanguageServer.TH


type CustomRequest = Method FromClient Request

pattern GetAllHoles :: CustomRequest
pattern GetAllHoles = CustomMethod

pattern SGetAllHoles :: SMethod (CustomMethod :: CustomRequest)
pattern SGetAllHoles = SCustomMethod "wingman/getAllHoles"

instance PluginMethod (CustomMethod :: CustomRequest) where
  pluginEnabled _ _ _ = True
  combineResponses _ _ _ _ (a :| _) = a


data GetAllHolesRequest = GetAllHolesRequest
  { _textDocument :: TextDocumentIdentifier
  }

deriveJSON lspOptions ''GetAllHolesRequest

data GetAllHolesResponse = GetAllHolesResponse
  { _holes :: [Range]
  }

deriveJSON lspOptions ''GetAllHolesResponse

mkGetAllHolesPluginHandler
    :: ( ideState
      -> PluginId
      -> GetAllHolesRequest
      -> LspM Ide.Plugin.Config.Config (Either ResponseError GetAllHolesResponse)
       )
    -> PluginHandlers ideState
mkGetAllHolesPluginHandler f = mkPluginHandler SGetAllHoles $ \state plId v -> do
  case fromJSON v of
    Success req -> fmap (fmap toJSON) $ f state plId req
    Error x -> pure $ Left $ ResponseError ParseError (T.pack x) Nothing

