module LogType (Log(..)) where

import qualified Development.IDE.Main              as IDE
import qualified Development.IDE.Plugin.HLS.GhcIde as Ghcide
import           Ide.Logger                        (Pretty (pretty))
import           Language.LSP.VFS                  (VfsLog)

data Log
  = LogGhcIde Ghcide.Log
  | LogIDEMain IDE.Log
  | LogVfs VfsLog

instance Pretty Log where
  pretty = \case
    LogGhcIde log  -> pretty log
    LogIDEMain log -> pretty log
    LogVfs log     -> pretty log
