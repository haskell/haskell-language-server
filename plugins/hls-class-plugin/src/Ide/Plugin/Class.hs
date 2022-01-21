{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.Class
  ( descriptor
  ) where

import           Control.Applicative
import           Control.Lens                            hiding (List, use)
import           Control.Monad
import           Control.Monad.IO.Class
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
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util         (BooleanFormula (..),
                                                          fsLit, listToBag)
import           Development.IDE.Spans.AtPoint
import qualified GHC.Generics                            as Generics
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Parser.Annotation                   hiding (locA)
#endif
import           Data.Either                             (fromRight)
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers (parseDecl)
import           Language.Haskell.GHC.ExactPrint.Types   hiding (GhcPs, Parens)
import           Language.Haskell.GHC.ExactPrint.Utils   (rs)
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens                 as J

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
#if MIN_VERSION_ghc(9,2,0)
      old = T.pack $ exactPrint (makeDeltaAst ps)
#else
      anns = relativiseApiAnns ps (pm_annotations pm)
      old = T.pack $ exactPrint ps anns
#endif

    (hsc_dflags . hscEnv -> df) <- MaybeT . runAction "classplugin" state $ use GhcSessionDeps docPath
#if MIN_VERSION_ghc(9,2,0)
    List (unzip -> (mBinds, mDecls)) <- MaybeT . pure $ traverse (makeMethodDecl df) methodGroup
    let
      (ps', _, _) = runTransform (addMethodDecls (makeDeltaAst ps) mBinds mDecls)
      new = T.pack $ exactPrint ps'
#else
    List (unzip -> (mAnns, mDecls)) <- MaybeT . pure $ traverse (makeMethodDecl df) methodGroup
    let
      (ps', (anns', _), _) = runTransform (mergeAnns (mergeAnnList mAnns) anns) (addMethodDecls ps mDecls)
      new = T.pack $ exactPrint ps' anns'
#endif

    pure (workspaceEdit caps old new)
  forM_ medit $ \edit ->
    sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  pure (Right Null)
  where

    indent = 2

    makeMethodDecl df mName =
      case parseDecl df (T.unpack mName) . T.unpack $ toMethodName mName <> " = _" of
#if MIN_VERSION_ghc(9,2,0)
        Right d@(L ld (ValD _ b)) -> Just (setEntryDP (makeDeltaAst (L ld b)) (DifferentLine 1 (indent + 1)), d)
        _                       -> Nothing
#else
        Right (ann, d) -> Just (setEntryDP d (DP (1, indent)) ann, d)
        Left _         -> Nothing
#endif

#if MIN_VERSION_ghc(9,2,0)
    insertAfter' (getLoc -> k) = insertAt findAfter
      where
        findAfter x xs =
          case span (\(L l _) -> locA l /= k) xs of
            ([],[])    -> [x]
            (fs,[])    -> fs++[x]
            (fs, b:bs) -> fs ++ (b : x : bs)

    addMethodDecls ps mBinds mDecls = do
      d <- findInstDecl ps
      newSpan <- uniqueSrcSpanT
      let
        ancWhere = Anchor (rs newSpan) (MovedAnchor (DifferentLine 1 30))
        ancMethod = Anchor (rs newSpan) (MovedAnchor (DifferentLine 1 (indent + 1)))
        ann = EpAnn ancWhere (AnnList (Just ancMethod) Nothing Nothing [AddEpAnn AnnWhere (EpaDelta (DifferentLine 1 50) [])] []) emptyComments
        mBinds' = makeDeltaAst (HsValBinds ann (ValBinds (captureOrder mDecls) (listToBag mBinds) []))
      mDecls' <- hsDeclsValBinds mBinds'
      mBinds'' <- replaceDeclsValbinds WithWhere mBinds' mDecls'
      mDecls'' <- hsDeclsValBinds mBinds''
      foldM (insertAfter' d) ps (reverse mDecls'')
#else
    addMethodDecls ps mDecls = do
      d <- findInstDecl ps
      newSpan <- uniqueSrcSpanT
      let
        annKey = mkAnnKey d
        newAnnKey = AnnKey (rs newSpan) (CN "HsValBinds")
        addWhere mkds@(Map.lookup annKey -> Just ann)
          = Map.insert newAnnKey ann2 mkds2
          where
            ann1 = ann
                   { annsDP = annsDP ann ++ [(G AnnWhere, DP (0, 1))]
                   , annCapturedSpan = Just newAnnKey
                   , annSortKey = Just (fmap (rs . getLoc) mDecls)
                   }
            mkds2 = Map.insert annKey ann1 mkds
            ann2 = annNone
                   { annEntryDelta = DP (1, indent)
                   }
        addWhere _ = panic "Ide.Plugin.Class.addMethodPlaceholder"
      modifyAnnsT addWhere
      modifyAnnsT (captureOrderAnnKey newAnnKey mDecls)
      foldM (insertAfter d) ps (reverse mDecls)
#endif

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
      x <- lift . traverse mkAction . minDefToMethodGroups . classMinimalDef $ cls
      pure x
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
          let
#if !MIN_VERSION_ghc(9,2,0)
            classIdentOf = head . head
#else
            classIdentOf = last . head
#endif
            instIdents =
              pointCommand hf (fromJust (fromCurrentRange pmap range) ^. J.start & J.character -~ 1)
#if !MIN_VERSION_ghc(9,0,0)
              ( (Map.keys . Map.filter isClassNodeIdentifier . nodeIdentifiers . nodeInfo)
                <=< nodeChildren
              )
#else
              ( (Map.keys . Map.filter isClassNodeIdentifier . sourcedNodeIdents . sourcedNodeInfo)
                <=< nodeChildren
              )
#endif
          in do
            pure $ classIdentOf instIdents

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
--                                  "• No explicit implementation for"

minDefToMethodGroups :: BooleanFormula Name -> [[T.Text]]
minDefToMethodGroups = go
  where
    go (Var mn)   = [[T.pack . occNameString . occName $ mn]]
    go (Or ms)    = concatMap (go . unLoc) ms
    go (And ms)   = foldr (liftA2 (<>)) [[]] (fmap (go . unLoc) ms)
    go (Parens m) = go (unLoc m)
