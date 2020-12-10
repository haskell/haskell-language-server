{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.Class
  ( descriptor
  ) where

import           BooleanFormula
import           Class
import           ConLike
import           Control.Applicative
import           Control.Lens hiding (List)
import           Control.Monad
import           Data.Aeson
import           Data.Char
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint
import qualified GHC.Generics as Generics
import           GhcPlugins hiding (Var, (<>))
import           Ide.Plugin
import           Ide.Types
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as J
import           TcEnv
import           TcRnMonad
import qualified Data.HashMap.Strict as H

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCommands = commands
  , pluginCodeActionProvider = Just codeAction
  }

commands :: [PluginCommand]
commands = [ PluginCommand "addMethodPlaceholders" "add placeholders for minimal methods" addMethodPlaceholders
           ]

-- | Parameter for the addMethods PluginCommand.
data AddMethodsParams = AddMethodsParams
  { uri         :: Uri
  , range       :: Range
  , methodGroup :: List T.Text
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

addMethodPlaceholders :: CommandFunction AddMethodsParams
addMethodPlaceholders _ _ AddMethodsParams{..} = pure (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams workspaceEdit))
  where
    workspaceEdit
      = WorkspaceEdit
        (Just (H.singleton uri textEdits))
        Nothing

    textEdits
      = List
        [ TextEdit (Range pos pos) $ "\n" <> methodText
        ]

    methodText
      = mconcat
      . intersperse "\n"
      . fmap (\n -> T.replicate indentSize " " <> toMethodName n <> " = _")
      . unList
      $ methodGroup

    toMethodName n
      | Just (h, _) <- T.uncons n
      , not (isAlpha h)
      = "(" <> n <> ")"
      | otherwise
      = n

    pos = range ^. J.end
    indentSize = range ^. J.start . J.character + 2

-- | This implementation is extremely ad-hoc in a sense that
-- 1. sensitive to the format of diagnostic messages from GHC
-- 2. pattern matches are not exhaustive
codeAction :: CodeActionProvider
codeAction _ state plId docId _ ctx = do
  let Just docPath = docId ^. J.uri & uriToFilePath <&> toNormalizedFilePath
  actions <- join <$> mapM (mkActions docPath) methodDiags
  pure . Right . List $ actions
  where
    ghcDiags = filter (\d -> d ^. J.source == Just "typecheck") . unList $ ctx ^. J.diagnostics
    methodDiags = filter (\d -> isClassMethodWarning (d ^. J.message)) ghcDiags

    ghostSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<haskell-language-sever>") 1 1

    mkAction range methodGroup
      = codeAction <$> mkLspCommand plId "addMethodPlaceholders" title (Just cmdParams)
      where
        title = "Add placeholders for "
          <> mconcat (intersperse ", " (fmap (\m -> "‘" <> m <> "’") methodGroup))
        cmdParams = [toJSON (AddMethodsParams (docId ^. J.uri) range (List methodGroup))]

        codeAction cmd
          = CACodeAction
          $ CodeAction title (Just CodeActionQuickFix) (Just (List [])) Nothing (Just cmd)

    mkActions docPath d = do
      Just (hieAst -> hf, _) <- runAction "classplugin" state $ useWithStale GetHieAst docPath
      let
        [([[Right name]], range)]
          = pointCommand hf (d ^. J.range . J.start & J.character -~ 1)
            $ \n ->
                ( Map.keys . Map.filter (isNothing . identType) . nodeIdentifiers . nodeInfo <$> nodeChildren n
                , realSrcSpanToRange (nodeSpan n)
                )
      Just (hscEnv -> hscenv, _) <- runAction "classplugin" state $ useWithStale GhcSessionDeps docPath
      Just (tmrTypechecked -> thisMod, _) <- runAction "classplugin" state $ useWithStale TypeCheck docPath
      (_, Just cls) <- initTcWithGbl hscenv thisMod ghostSpan $ do
        tcthing <- tcLookup name
        case tcthing of
          AGlobal (AConLike (RealDataCon con))
            | Just cls <- tyConClass_maybe (dataConOrigTyCon con) -> pure cls
          _ -> panic "Ide.Plugin.Class.mkActions"
      let
        minDef = classMinimalDef cls
      traverse (mkAction range) (minDefToMethodGroups minDef)

unList :: List a -> [a]
unList (List xs) = xs

isClassMethodWarning :: T.Text -> Bool
isClassMethodWarning = T.isPrefixOf "• No explicit implementation for"

minDefToMethodGroups :: BooleanFormula Name -> [[T.Text]]
minDefToMethodGroups = go
  where
    go (Var mn) = [[T.pack (occNameString (occName mn))]]
    go (Or ms) = concatMap (go . unLoc) ms
    go (And ms) = foldr (liftA2 (<>)) [[]] (fmap (go . unLoc) ms)
    go (Parens m) = go (unLoc m)
