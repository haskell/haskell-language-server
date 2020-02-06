module Utils where

import           Data.Default
import qualified Language.Haskell.LSP.Test as Test
import           Language.Haskell.LSP.Test hiding (message)
import qualified Language.Haskell.LSP.Types.Capabilities as C

-- ---------------------------------------------------------------------

noLogConfig :: SessionConfig
noLogConfig = Test.defaultConfig { logMessages = False }

logConfig :: SessionConfig
logConfig = Test.defaultConfig { logMessages = True }

codeActionSupportCaps :: C.ClientCapabilities
codeActionSupportCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = C.CodeActionClientCapabilities (Just True) (Just literalSupport)
    literalSupport = C.CodeActionLiteralSupport def
