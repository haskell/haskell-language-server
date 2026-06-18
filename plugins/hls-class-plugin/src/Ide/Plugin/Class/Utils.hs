{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Class.Utils where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import           Data.Char                        (isAlpha)
import qualified Data.Text                        as T
import           Development.IDE
import           Development.IDE.Core.InputPath
import           Development.IDE.Core.PluginUtils
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util  (fsLit)
import           Development.IDE.Spans.Pragmas    (getNextPragmaInfo,
                                                   insertNewPragma)
import           Ide.Plugin.Error
import           Ide.PluginUtils
import           Language.LSP.Protocol.Types

-- | Check if some `HasSrcSpan` value is in the given range
inRange :: Range -> SrcSpan -> Bool
inRange range s = maybe False (subRange range) (srcSpanToRange s)

ghostSpan :: RealSrcSpan
ghostSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<haskell-language-sever>") 1 1

showDoc :: HscEnv -> TcGblEnv -> Type -> String
showDoc hsc gblEnv ty = showSDocForUser' hsc (mkPrintUnqualifiedDefault hsc (rdrEnv gblEnv)) (pprSigmaType ty)
    where rdrEnv gblEnv = tcg_rdr_env gblEnv

-- | Paren the name for pretty display if necessary
toMethodName :: T.Text -> T.Text
toMethodName n
    | Just (h, _) <- T.uncons n
    , not (isAlpha h || h == '_')
    = "(" <> n <> ")"
    | otherwise
    = n

-- | Here we use `useWithStale` to compute, Using stale results means that we can almost always return a value.
--   In practice this means the lenses don't 'flicker'.
--   This function is also used in code actions, but it doesn't matter because our actions only work
--   if the module parsed success.
insertPragmaIfNotPresent :: (MonadIO m)
    => IdeState
    -> NormalizedFilePath
    -> Extension
    -> ExceptT PluginError m [TextEdit]
insertPragmaIfNotPresent state nfp pragma = do
    (hscEnv -> hsc_dflags -> sessionDynFlags, _) <- runActionE "classplugin.insertPragmaIfNotPresent.GhcSession" state
        $ useWithStaleE GhcSession nfp
    fileContents <- liftIO $ runAction "classplugin.insertPragmaIfNotPresent.GetFileContents" state
        $ getFileContents $ toAllHaskellInput nfp
    (pm, _) <- runActionE "classplugin.insertPragmaIfNotPresent.GetParsedModuleWithComments" state
        $ useWithStaleE GetParsedModuleWithComments nfp
    let exts = getExtensions pm
        info = getNextPragmaInfo sessionDynFlags fileContents
    pure [insertNewPragma info pragma | pragma `notElem` exts]
