{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Class.Utils where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import           Data.Char                       (isAlpha)
import           Data.List                       (isPrefixOf)
import           Data.String                     (IsString)
import qualified Data.Text                       as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Spans.Pragmas   (getNextPragmaInfo,
                                                  insertNewPragma)
import           Ide.PluginUtils
import           Language.LSP.Types

-- | All instance bindings are started with `$c`
bindingPrefix :: IsString s => s
bindingPrefix = "$c"

isBindingName :: Name -> Bool
isBindingName name = isPrefixOf bindingPrefix $ occNameString $ nameOccName name

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

-- | Paren the name for pretty display if necessary
toMethodName :: T.Text -> T.Text
toMethodName n
    | Just (h, _) <- T.uncons n
    , not (isAlpha h || h == '_')
    = "(" <> n <> ")"
    | otherwise
    = n

insertPragmaIfNotPresent :: (MonadIO m)
    => IdeState
    -> NormalizedFilePath
    -> Extension
    -> ExceptT String m [TextEdit]
insertPragmaIfNotPresent state nfp pragma = do
    (hscEnv -> hsc_dflags -> sessionDynFlags, _) <- handleMaybeM "Unable to get GhcSession"
        $ liftIO
        $ runAction "classplugin.insertPragmaIfNotPresent.GhcSession" state
        $ useWithStale GhcSession nfp
    (_, fileContents) <- liftIO
        $ runAction "classplugin.insertPragmaIfNotPresent.GetFileContents" state
        $ getFileContents nfp
    pm <- handleMaybeM "Unable to GetParsedModuleWithComments"
        $ liftIO
        $ runAction "classplugin.insertPragmaIfNotPresent.GetParsedModuleWithComments" state
        $ use GetParsedModuleWithComments nfp
    let exts = getExtensions pm
        info = getNextPragmaInfo sessionDynFlags fileContents
    pure [insertNewPragma info pragma | pragma `notElem` exts]
