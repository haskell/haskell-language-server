{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.HaddockComments (descriptor, E.Log) where


import           Control.Monad                         (join, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class             (lift)
import qualified Data.Map                              as Map
import qualified Data.Text                             as T
import           Development.IDE                       hiding (pluginHandlers)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.ExactPrint        (GetAnnotatedParsedSource (..))
import qualified Development.IDE.GHC.ExactPrint        as E
import           Development.IDE.Plugin.CodeAction
-- import           Ide.Plugin.HaddockComments.Data       (genForDataDecl)
-- import           Ide.Plugin.HaddockComments.Prelude
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Types hiding (GhcPs)
import           Language.Haskell.GHC.ExactPrint.Utils
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

-----------------------------------------------------------------------------
descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = mkExactprintPluginDescriptor recorder $
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction codeActionProvider
    }

codeActionProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeActionProvider ideState _pId (CodeActionParams _ _ (TextDocumentIdentifier uri) range CodeActionContext {_diagnostics = diags}) =
  do
    let noErr = and $ (/= Just DiagnosticSeverity_Error) . _severity <$> diags
        nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    (fmap astA . join -> pm) <- liftIO $ runAction "HaddockComments.GetAnnotatedParsedSource" ideState $ use GetAnnotatedParsedSource `traverse` nfp
    case pm of
      Nothing -> pure . Right $ InL []
      Just (ast :: ParsedSource) -> pure . Right $ InL []
