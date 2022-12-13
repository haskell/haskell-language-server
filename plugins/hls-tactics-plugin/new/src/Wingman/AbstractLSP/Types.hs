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
import           Development.IDE.GHC.ExactPrint (Graft)
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat hiding (Target)
import           GHC.Generics (Generic)
import qualified Ide.Plugin.Config as Plugin
import           Ide.Types hiding (Config)
import           Language.LSP.Server (LspM)
import           Language.LSP.Types hiding (CodeLens, CodeAction)
import           Wingman.LanguageServer (judgementForHole)
import           Wingman.Types


------------------------------------------------------------------------------
-- | An 'Interaction' is an existential 'Continuation', which handles both
-- sides of the request/response interaction for LSP.
data Interaction where
  Interaction
      :: (IsTarget target, IsContinuationSort sort, A.ToJSON b, A.FromJSON b)
      => Continuation sort target b
      -> Interaction


------------------------------------------------------------------------------
-- | Metadata for a command. Used by both code actions and lenses, though for
-- lenses, only 'md_title' is currently used.
data Metadata
  = Metadata
      { md_title     :: Text
      , md_kind      :: CodeActionKind
      , md_preferred :: Bool
      }
  deriving stock (Eq, Show)


------------------------------------------------------------------------------
-- | Whether we're defining a CodeAction or CodeLens.
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


------------------------------------------------------------------------------
-- | Transform a "continuation sort" into a 'CommandId'.
class IsContinuationSort a where
  toCommandId :: a -> CommandId

instance IsContinuationSort CommandId where
  toCommandId = id

instance IsContinuationSort Text where
  toCommandId = CommandId


------------------------------------------------------------------------------
-- | Ways a 'Continuation' can resolve.
data ContinuationResult
  = -- | Produce some error messages.
    ErrorMessages [UserFacingMessage]
    -- | Produce an explicit 'WorkspaceEdit'.
  | RawEdit WorkspaceEdit
    -- | Produce a 'Graft', corresponding to a transformation of the current
    -- AST.
  | GraftEdit (Graft (Either String) ParsedSource)


------------------------------------------------------------------------------
-- | A 'Continuation' is a single object corresponding to an action that users
-- can take via LSP. It generalizes codeactions and codelenses, allowing for
-- a significant amount of code reuse.
--
-- Given @Continuation sort target payload@:
--
-- the @sort@ corresponds to a 'CommandId', allowing you to namespace actions
-- rather than working directly with text. This functionality is driven via
-- 'IsContinuationSort'.
--
-- the @target@ is used to fetch data from LSP on both sides of the
-- request/response barrier. For example, you can use it to resolve what node
-- in the AST the incoming range refers to. This functionality is driven via
-- 'IsTarget'.
--
-- the @payload@ is used for data you'd explicitly like to send from the
-- request to the response. It's like @target@, but only gets computed once.
-- This is beneficial if you can do it, but requires that your data is
-- serializable via JSON.
data Continuation sort target payload = Continuation
  { c_sort :: sort
  , c_makeCommand :: SynthesizeCommand target payload
  , c_runCommand
        :: LspEnv
        -> TargetArgs target
        -> FileContext
        -> payload
        -> MaybeT (LspM Plugin.Config) [ContinuationResult]
  }


------------------------------------------------------------------------------
-- | What file are we looking at, and what bit of it?
data FileContext = FileContext
  { fc_uri      :: Uri
  , fc_range    :: Maybe (Tracked 'Current Range)
    -- ^ For code actions, this is 'Just'. For code lenses, you'll get
    -- a 'Nothing' in the request, and a 'Just' in the response.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)


------------------------------------------------------------------------------
-- | Everything we need to resolve continuations.
data LspEnv = LspEnv
  { le_ideState    :: IdeState
  , le_pluginId    :: PluginId
  , le_dflags      :: DynFlags
  , le_config      :: Config
  , le_fileContext :: FileContext
  }


------------------------------------------------------------------------------
-- | Extract some information from LSP, so it can be passed to the requests and
-- responses of a 'Continuation'.
class IsTarget t where
  type TargetArgs t
  fetchTargetArgs
      :: LspEnv
      -> MaybeT (LspM Plugin.Config) (TargetArgs t)

------------------------------------------------------------------------------
-- | A 'HoleTarget' is a target (see 'IsTarget') which succeeds if the given
-- range is an HsExpr hole. It gives continuations access to the resulting
-- tactic judgement.
data HoleTarget = HoleTarget
  deriving stock (Eq, Ord, Show, Enum, Bounded)

getNfp :: Applicative m => Uri -> MaybeT m NormalizedFilePath
getNfp = MaybeT . pure . uriToNormalizedFilePath . toNormalizedUri

instance IsTarget HoleTarget where
  type TargetArgs HoleTarget = HoleJudgment
  fetchTargetArgs LspEnv{..} = do
    let FileContext{..} = le_fileContext
    range <- MaybeT $ pure fc_range
    nfp <- getNfp fc_uri
    mapMaybeT liftIO $ judgementForHole le_ideState nfp range le_config

