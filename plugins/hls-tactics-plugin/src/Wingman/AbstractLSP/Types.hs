{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Wingman.AbstractLSP.Types where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), mapMaybeT)
import qualified Data.Aeson as A
import           Data.Text (Text)
import           Development.IDE (IdeState)
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat hiding (Target)
import           GHC.Generics (Generic)
import qualified Ide.Plugin.Config as Plugin
import           Ide.Types
import           Language.LSP.Server (LspM)
import           Language.LSP.Types hiding (CodeLens, CodeAction)
import           Wingman.LanguageServer (judgementForHole)
import           Wingman.Types


data Interaction where
  Interaction
      :: (IsTarget target, Show sort, A.ToJSON b, A.FromJSON b)
      => Continuation sort target b
      -> Interaction


data Metadata
  = Metadata
      { md_title     :: Text
      , md_kind      :: CodeActionKind
      , md_preferred :: Bool
      }
  deriving stock (Eq, Show)


data SynthesizeCommand a b
  = SynthesizeCodeAction
      ( LspEnv
     -> TargetArgs a
     -> MaybeT (LspM Plugin.Config) [(Metadata, b)]
      )
  | SynthesizeCodeLens
      ( LspEnv
     -> TargetArgs a
     -> MaybeT (LspM Plugin.Config) [(Range, Metadata, b)]
      )


data Continuation sort a b = Continuation
  { c_sort :: sort
  , c_makeCommand :: SynthesizeCommand a b
  , c_runCommand
        :: LspEnv
        -> TargetArgs a
        -> FileContext
        -> b
        -> MaybeT (LspM Plugin.Config)
                  (Either [UserFacingMessage] WorkspaceEdit)
  }


data HoleTarget = HoleTarget
  deriving stock (Eq, Ord, Show, Enum, Bounded)


data FileContext = FileContext
  { fc_uri      :: Uri
  , fc_nfp      :: NormalizedFilePath
  , fc_range    :: Maybe (Tracked 'Current Range)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

deriving anyclass instance A.ToJSON NormalizedFilePath
deriving anyclass instance A.FromJSON NormalizedFilePath
deriving anyclass instance A.ToJSON NormalizedUri
deriving anyclass instance A.FromJSON NormalizedUri


data LspEnv = LspEnv
  { le_ideState    :: IdeState
  , le_pluginId    :: PluginId
  , le_dflags      :: DynFlags
  , le_config      :: Config
  , le_fileContext :: FileContext
  }

class IsTarget t where
  type TargetArgs t
  fetchTargetArgs
      :: LspEnv
      -> MaybeT (LspM Plugin.Config) (TargetArgs t)

instance IsTarget HoleTarget where
  type TargetArgs HoleTarget = HoleJudgment
  fetchTargetArgs LspEnv{..} = do
    let FileContext{..} = le_fileContext
    range <- MaybeT $ pure fc_range
    mapMaybeT liftIO $ judgementForHole le_ideState fc_nfp range le_config

