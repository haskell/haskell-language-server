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
import           Control.Lens                            hiding (List, use)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Char
import           Data.List
import qualified Data.Map.Strict                         as Map
import           Data.Maybe
import qualified Data.Text                               as T
import           Development.IDE                         hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping    (fromCurrentRange,
                                                          toCurrentRange)
import           Development.IDE.GHC.Compat              hiding (getLoc)
import           Development.IDE.Spans.AtPoint
import qualified GHC.Generics                            as Generics
import           GhcPlugins                              hiding (Var, getLoc,
                                                          (<>))
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers (parseDecl)
import           Language.Haskell.GHC.ExactPrint.Types   hiding (GhcPs, Parens)
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens                 as J
import           SrcLoc
import           TcEnv
import           TcRnMonad

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCommands = commands
  , pluginHandlers = mkPluginHandler STextDocumentCodeAction codeAction
  }

commands :: [PluginCommand IdeState]
commands
  = [ PluginCommand "addMinimalMethodPlaceholders" "add placeholders for minimal methods" addMethodPlaceholders
    ]

-- | Parameter for the addMethods PluginCommand.
data AddMinimalMethodsParams = AddMinimalMethodsParams
  { uri         :: Uri
  , range       :: Range
  , methodGroup :: List T.Text
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

addMethodPlaceholders :: CommandFunction IdeState AddMinimalMethodsParams
addMethodPlaceholders state AddMinimalMethodsParams{..} = do
  caps <- getClientCapabilities
  medit <- liftIO $ runMaybeT $ do
    docPath <- MaybeT . pure . uriToNormalizedFilePath $ toNormalizedUri uri
    pm <- MaybeT . runAction "classplugin" state $ use GetParsedModule docPath
    let
      ps = pm_parsed_source pm
      anns = relativiseApiAnns ps (pm_annotations pm)
      old = T.pack $ exactPrint ps anns

    (hsc_dflags . hscEnv -> df) <- MaybeT . runAction "classplugin" state $ use GhcSessionDeps docPath
    List (unzip -> (mAnns, mDecls)) <- MaybeT . pure $ traverse (makeMethodDecl df) methodGroup
    let
      (ps', (anns', _), _) = runTransform (mergeAnns (mergeAnnList mAnns) anns) (addMethodDecls ps mDecls)
      new = T.pack $ exactPrint ps' anns'

    pure (workspaceEdit caps old new)
  forM_ medit $ \edit ->
    sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  pure (Right Null)
  where

    indent = 2

    makeMethodDecl df mName =
      case parseDecl df (T.unpack mName) . T.unpack $ toMethodName mName <> " = _" of
        Right (ann, d) -> Just (setPrecedingLines d 1 indent ann, d)
        Left _         -> Nothing

    addMethodDecls :: ParsedSource -> [LHsDecl GhcPs] -> Transform (Located (HsModule GhcPs))
    addMethodDecls ps mDecls = do
      d <- findInstDecl ps
      newSpan <- uniqueSrcSpanT
      let
        annKey = mkAnnKey d
        newAnnKey = AnnKey newSpan (CN "HsValBinds")
        addWhere mkds@(Map.lookup annKey -> Just ann)
          = Map.insert newAnnKey ann2 mkds2
          where
            ann1 = ann
                   { annsDP = annsDP ann ++ [(G AnnWhere, DP (0, 1))]
                   , annCapturedSpan = Just newAnnKey
                   , annSortKey = Just (fmap getLoc mDecls)
                   }
            mkds2 = Map.insert annKey ann1 mkds
            ann2 = annNone
                   { annEntryDelta = DP (1, indent)
                   }
        addWhere _ = panic "Ide.Plugin.Class.addMethodPlaceholder"
      modifyAnnsT addWhere
      modifyAnnsT (captureOrderAnnKey newAnnKey mDecls)
      foldM (insertAfter d) ps (reverse mDecls)

    findInstDecl :: ParsedSource -> Transform (LHsDecl GhcPs)
    findInstDecl ps = head . filter (containRange range . getLoc) <$> hsDecls ps

    workspaceEdit caps old new
      = diffText caps (uri, old) new IncludeDeletions

    toMethodName n
      | Just (h, _) <- T.uncons n
      , not (isAlpha h || h == '_')
      = "(" <> n <> ")"
      | otherwise
      = n

-- |
-- This implementation is ad-hoc in a sense that the diagnostic detection mechanism is
-- sensitive to the format of diagnostic messages from GHC.
codeAction :: PluginMethodHandler IdeState TextDocumentCodeAction
codeAction state plId (CodeActionParams _ _ docId _ context) = liftIO $ fmap (fromMaybe errorResult) . runMaybeT $ do
  docPath <- MaybeT . pure . uriToNormalizedFilePath $ toNormalizedUri uri
  actions <- join <$> mapM (mkActions docPath) methodDiags
  pure . Right . List $ actions
  where
    errorResult = Right (List [])
    uri = docId ^. J.uri
    List diags = context ^. J.diagnostics

    ghcDiags = filter (\d -> d ^. J.source == Just "typecheck") diags
    methodDiags = filter (\d -> isClassMethodWarning (d ^. J.message)) ghcDiags

    mkActions docPath diag = do
      ident <- findClassIdentifier docPath range
      cls <- findClassFromIdentifier docPath ident
      lift . traverse mkAction . minDefToMethodGroups . classMinimalDef $ cls
      where
        range = diag ^. J.range

        mkAction methodGroup
          = pure $ mkCodeAction title $ mkLspCommand plId "addMinimalMethodPlaceholders" title (Just cmdParams)
          where
            title = mkTitle methodGroup
            cmdParams = mkCmdParams methodGroup

        mkTitle methodGroup
          = "Add placeholders for "
          <> mconcat (intersperse ", " (fmap (\m -> "'" <> m <> "'") methodGroup))

        mkCmdParams methodGroup = [toJSON (AddMinimalMethodsParams uri range (List methodGroup))]

        mkCodeAction title cmd
          = InR
          $ CodeAction title (Just CodeActionQuickFix) (Just (List [])) Nothing Nothing Nothing (Just cmd) Nothing

    findClassIdentifier docPath range = do
      (hieAstResult, pmap) <- MaybeT . runAction "classplugin" state $ useWithStale GetHieAst docPath
      case hieAstResult of
        HAR {hieAst = hf} ->
          pure
            $ head . head
            $ pointCommand hf (fromJust (fromCurrentRange pmap range) ^. J.start & J.character -~ 1)
              ( (Map.keys . Map.filter isClassNodeIdentifier . nodeIdentifiers . nodeInfo)
                <=< nodeChildren
              )

    findClassFromIdentifier docPath (Right name) = do
      (hscEnv -> hscenv, _) <- MaybeT . runAction "classplugin" state $ useWithStale GhcSessionDeps docPath
      (tmrTypechecked -> thisMod, _) <- MaybeT . runAction "classplugin" state $ useWithStale TypeCheck docPath
      MaybeT . fmap snd . initTcWithGbl hscenv thisMod ghostSpan $ do
        tcthing <- tcLookup name
        case tcthing of
          AGlobal (AConLike (RealDataCon con))
            | Just cls <- tyConClass_maybe (dataConOrigTyCon con) -> pure cls
          _ -> panic "Ide.Plugin.Class.findClassFromIdentifier"
    findClassFromIdentifier _ (Left _) = panic "Ide.Plugin.Class.findClassIdentifier"

ghostSpan :: RealSrcSpan
ghostSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<haskell-language-sever>") 1 1

containRange :: Range -> SrcSpan -> Bool
containRange range x = isInsideSrcSpan (range ^. J.start) x || isInsideSrcSpan (range ^. J.end) x

isClassNodeIdentifier :: IdentifierDetails a -> Bool
isClassNodeIdentifier = isNothing . identType

isClassMethodWarning :: T.Text -> Bool
isClassMethodWarning = T.isPrefixOf "• No explicit implementation for"

minDefToMethodGroups :: BooleanFormula Name -> [[T.Text]]
minDefToMethodGroups = go
  where
    go (Var mn)   = [[T.pack . occNameString . occName $ mn]]
    go (Or ms)    = concatMap (go . unLoc) ms
    go (And ms)   = foldr (liftA2 (<>)) [[]] (fmap (go . unLoc) ms)
    go (Parens m) = go (unLoc m)
