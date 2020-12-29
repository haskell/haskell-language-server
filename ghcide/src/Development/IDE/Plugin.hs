
module Development.IDE.Plugin(Plugin(..), codeActionPlugin, codeActionPluginWithRules,makeLspCommandId,getPid) where

import Data.Default
import qualified Data.Text as T
import Development.Shake
import Development.IDE.LSP.Server

import           Language.Haskell.LSP.Types
import Development.IDE.Compat
import Development.IDE.Core.Rules
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages


data Plugin c = Plugin
    {pluginRules :: Rules ()
    ,pluginHandler :: PartialHandlers c
    }

instance Default (Plugin c) where
    def = Plugin mempty def

instance Semigroup (Plugin c) where
    Plugin x1 y1 <> Plugin x2 y2 = Plugin (x1<>x2) (y1<>y2)

instance Monoid (Plugin c) where
    mempty = def


codeActionPlugin :: (LSP.LspFuncs c -> IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> IO (Either ResponseError [CAResult])) -> Plugin c
codeActionPlugin = codeActionPluginWithRules mempty

codeActionPluginWithRules :: Rules () -> (LSP.LspFuncs c -> IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> IO (Either ResponseError [CAResult])) -> Plugin c
codeActionPluginWithRules rr f = Plugin rr $ PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeActionHandler = withResponse RspCodeAction g
    }
    where
      g lsp state (CodeActionParams a b c _) = fmap List <$> f lsp state a b c

-- | Prefix to uniquely identify commands sent to the client.  This
-- has two parts
--
-- - A representation of the process id to make sure that a client has
--   unique commands if it is running multiple servers, since some
--   clients have a global command table and get confused otherwise.
--
-- - A string to identify ghcide, to ease integration into
--   haskell-language-server, which routes commands to plugins based
--   on that.
makeLspCommandId :: T.Text -> IO T.Text
makeLspCommandId command = do
    pid <- getPid
    return $ pid <> ":ghcide:" <> command

-- | Get the operating system process id for the running server
-- instance. This should be the same for the lifetime of the instance,
-- and different from that of any other currently running instance.
getPid :: IO T.Text
getPid = T.pack . show <$> getProcessID
