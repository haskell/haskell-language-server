{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A plugin that uses tactics to synthesize code
module Wingman.Plugin
  ( descriptor
  , tacticTitle
  , TacticCommand (..)
  ) where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import qualified Data.Text as T
import           Development.IDE.Core.Shake (IdeState (..))
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           Prelude hiding (span)
import           System.Timeout
import           Wingman.AbstractLSP
import           Wingman.AbstractLSP.TacticActions (makeTacticCodeAction)
import           Wingman.AbstractLSP.Types (Interaction(Interaction))
import           Wingman.EmptyCase
import           Wingman.LanguageServer
import           Wingman.LanguageServer.Metaprogram (hoverProvider)
import           Wingman.StaticPlugin
import           Wingman.Types


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId
  = installInteractions
      ( Interaction testInteraction
      : fmap (Interaction . makeTacticCodeAction) [minBound .. maxBound]
      )
  $ (defaultPluginDescriptor plId)
      { pluginCommands
          = mconcat
              [ pure $
                  PluginCommand
                  emptyCaseLensCommandId
                  "Complete the empty case"
                  workspaceEditHandler
              ]
      , pluginHandlers = mconcat
          [ mkPluginHandler STextDocumentCodeLens codeLensProvider
          , mkPluginHandler STextDocumentHover hoverProvider
          ]
      , pluginRules = wingmanRules plId
      , pluginConfigDescriptor =
          defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
      , pluginModifyDynflags = staticPlugin
      }


showUserFacingMessage
    :: MonadLsp cfg m
    => UserFacingMessage
    -> m (Either ResponseError a)
showUserFacingMessage ufm = do
  showLspMessage $ mkShowMessageParams ufm
  pure $ Left $ mkErr InternalError $ T.pack $ show ufm


------------------------------------------------------------------------------
-- | The number of microseconds in a second
seconds :: Num a => a
seconds = 1e6


timingOut
    :: Int  -- ^ Time in microseconds
    -> IO a    -- ^ Computation to run
    -> MaybeT IO a
timingOut t m = MaybeT $ timeout t m


mkErr :: ErrorCode -> T.Text -> ResponseError
mkErr code err = ResponseError code err Nothing


