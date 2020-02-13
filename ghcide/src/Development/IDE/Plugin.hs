
module Development.IDE.Plugin(Plugin(..), codeActionPlugin) where

import Data.Default
import Development.Shake
import Development.IDE.LSP.Server

import           Language.Haskell.LSP.Types
import Development.IDE.Core.Rules
import           Development.IDE.Core.IdeConfiguration
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages


data Plugin = Plugin
    {pluginRules :: Rules ()
    ,pluginHandler :: PartialHandlers
    }

instance Default Plugin where
    def = Plugin mempty def

instance Semigroup Plugin where
    Plugin x1 y1 <> Plugin x2 y2 = Plugin (x1<>x2) (y1<>y2)

instance Monoid Plugin where
    mempty = def


codeActionPlugin :: (LSP.LspFuncs IdeConfiguration -> IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> IO (Either ResponseError [CAResult])) -> Plugin
codeActionPlugin f = Plugin mempty $ PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeActionHandler = withResponse RspCodeAction g
    }
    where
      g lsp state (CodeActionParams a b c _) = fmap List <$> f lsp state a b c
