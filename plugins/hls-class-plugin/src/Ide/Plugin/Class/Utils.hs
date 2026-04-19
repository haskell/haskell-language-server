{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Class.Utils where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import           Data.Char                        (isAlpha, isDigit)
import           Data.List                        (isPrefixOf)
import           Data.String                      (IsString)
import qualified Data.Text                        as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Spans.Pragmas    (getNextPragmaInfo,
                                                   insertNewPragma)
import           Ide.Plugin.Error
import           Ide.PluginUtils
import           Language.LSP.Protocol.Types

-- | All instance bindings are started with `$c`
bindingPrefix :: IsString s => s
bindingPrefix = "$c"

-- | Superclasses generate bindings in typeclasses as well.
--
-- When determining which bindings to create placeholders for, these
-- superclass-generated names need to be excluded.
-- TODO: This function should be replaced by an equivalent one from GHC:
--   https://gitlab.haskell.org/ghc/ghc/-/issues/27195
isSuperClassesBindingPrefix :: String -> Bool
isSuperClassesBindingPrefix ('$' : 'c' : 'p' : n : _) | isDigit n = True
isSuperClassesBindingPrefix _ = False

isBindingName :: Name -> Bool
isBindingName name =
  let bindingName = occNameString $ nameOccName name
   in isPrefixOf bindingPrefix bindingName && not (isSuperClassesBindingPrefix bindingName)

-- | Check if some `HasSrcSpan` value in the given range
inRange :: Range -> SrcSpan -> Bool
inRange range s = maybe False (subRange range) (srcSpanToRange s)

ghostSpan :: RealSrcSpan
ghostSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<haskell-language-sever>") 1 1

-- | "$cname" ==> "name"
prettyBindingNameString :: T.Text -> T.Text
prettyBindingNameString name
    | T.isPrefixOf bindingPrefix name =
        toMethodName $ T.drop (T.length bindingPrefix) name
    | otherwise = name

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
        $ getFileContents nfp
    (pm, _) <- runActionE "classplugin.insertPragmaIfNotPresent.GetParsedModuleWithComments" state
        $ useWithStaleE GetParsedModuleWithComments nfp
    let exts = getExtensions pm
        info = getNextPragmaInfo sessionDynFlags fileContents
    pure [insertNewPragma info pragma | pragma `notElem` exts]
