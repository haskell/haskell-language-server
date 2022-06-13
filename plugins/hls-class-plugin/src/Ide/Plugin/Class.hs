{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.Class
  ( descriptor,
    Log (..)
  ) where

import           Control.Applicative
import           Control.Lens                            hiding (List, use)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Char
import           Data.Either                             (rights)
import           Data.List
import qualified Data.Map.Strict                         as Map
import           Data.Maybe
import qualified Data.Set                                as Set
import qualified Data.Text                               as T
import           Development.IDE                         hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping    (fromCurrentRange,
                                                          toCurrentRange)
import           Development.IDE.GHC.Compat              as Compat hiding (locA,
                                                                    (<+>))
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Spans.AtPoint
import qualified GHC.Generics                            as Generics
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers (parseDecl)
import           Language.Haskell.GHC.ExactPrint.Types   hiding (GhcPs, Parens)
import           Language.Haskell.GHC.ExactPrint.Utils   (rs)
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens                 as J

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Hs                                  (AnnsModule (AnnsModule))
import           GHC.Parser.Annotation
#endif

data Log
  = LogImplementedMethods Class [T.Text]

instance Pretty Log where
  pretty = \case
    LogImplementedMethods cls methods ->
      pretty ("Detected implmented methods for class" :: String)
        <+> pretty (show (getOccString cls) <> ":") -- 'show' is used here to add quotes around the class name
        <+> pretty methods

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginCommands = commands
  , pluginHandlers = mkPluginHandler STextDocumentCodeAction (codeAction recorder)
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
    (hsc_dflags . hscEnv -> df) <- MaybeT . runAction "classplugin" state $ use GhcSessionDeps docPath
    (old, new) <- makeEditText pm df
    pure (workspaceEdit caps old new)

  forM_ medit $ \edit ->
    sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  pure (Right Null)
  where
    indent = 2

    workspaceEdit caps old new
      = diffText caps (uri, old) new IncludeDeletions

    toMethodName n
      | Just (h, _) <- T.uncons n
      , not (isAlpha h || h == '_')
      = "(" <> n <> ")"
      | otherwise
      = n

#if MIN_VERSION_ghc(9,2,0)
    makeEditText pm df = do
      List mDecls <- MaybeT . pure $ traverse (makeMethodDecl df) methodGroup
      let ps = makeDeltaAst $ pm_parsed_source pm
          old = T.pack $ exactPrint ps
          (ps', _, _) = runTransform (addMethodDecls ps mDecls)
          new = T.pack $ exactPrint ps'
      pure (old, new)

    makeMethodDecl df mName =
        either (const Nothing) Just . parseDecl df (T.unpack mName) . T.unpack
            $ toMethodName mName <> " = _"

    addMethodDecls ps mDecls = do
      allDecls <- hsDecls ps
      let (before, ((L l inst): after)) = break (containRange range . getLoc) allDecls
      replaceDecls ps (before ++ (L l (addWhere inst)): (map newLine mDecls ++ after))
      where
        -- Add `where` keyword for `instance X where` if `where` is missing.
        --
        -- The `where` in ghc-9.2 is now stored in the instance declaration
        --   directly. More precisely, giving an `HsDecl GhcPs`, we have:
        --   InstD --> ClsInstD --> ClsInstDecl --> XCClsInstDecl --> (EpAnn [AddEpAnn], AnnSortKey),
        --   here `AnnEpAnn` keeps the track of Anns.
        --
        -- See the link for the original definition:
        --   https://hackage.haskell.org/package/ghc-9.2.1/docs/Language-Haskell-Syntax-Extension.html#t:XCClsInstDecl
        addWhere (InstD xInstD (ClsInstD ext decl@ClsInstDecl{..})) =
          let ((EpAnn entry anns comments), key) = cid_ext
          in InstD xInstD (ClsInstD ext decl {
            cid_ext = (EpAnn
                        entry
                        (AddEpAnn AnnWhere (EpaDelta (SameLine 1) []) : anns)
                        comments
                      , key)
          })
        addWhere decl = decl

        newLine (L l e) =
          let dp = deltaPos 1 indent
          in L (noAnnSrcSpanDP (locA l) dp <> l) e

#else
    makeEditText pm df = do
      List (unzip -> (mAnns, mDecls)) <- MaybeT . pure $ traverse (makeMethodDecl df) methodGroup
      let ps = pm_parsed_source pm
          anns = relativiseApiAnns ps (pm_annotations pm)
          old = T.pack $ exactPrint ps anns
          (ps', (anns', _), _) = runTransform (mergeAnns (mergeAnnList mAnns) anns) (addMethodDecls ps mDecls)
          new = T.pack $ exactPrint ps' anns'
      pure (old, new)

    makeMethodDecl df mName =
      case parseDecl df (T.unpack mName) . T.unpack $ toMethodName mName <> " = _" of
        Right (ann, d) -> Just (setPrecedingLines d 1 indent ann, d)
        Left _         -> Nothing

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

    findInstDecl :: ParsedSource -> Transform (LHsDecl GhcPs)
    findInstDecl ps = head . filter (containRange range . getLoc) <$> hsDecls ps
#endif

-- |
-- This implementation is ad-hoc in a sense that the diagnostic detection mechanism is
-- sensitive to the format of diagnostic messages from GHC.
codeAction :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState TextDocumentCodeAction
codeAction recorder state plId (CodeActionParams _ _ docId _ context) = liftIO $ fmap (fromMaybe errorResult) . runMaybeT $ do
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
      (HAR {hieAst = ast}, pmap) <-
          MaybeT . runAction "classplugin" state $ useWithStale GetHieAst docPath
      instancePosition <- MaybeT . pure $
                          fromCurrentRange pmap range ^? _Just . J.start
                          & fmap (J.character -~ 1)

      ident <- findClassIdentifier ast instancePosition
      cls <- findClassFromIdentifier docPath ident
      implemented <- findImplementedMethods ast instancePosition
      logWith recorder Info (LogImplementedMethods cls implemented)
      lift . traverse (mkAction . (\\ implemented)) . minDefToMethodGroups . classMinimalDef $ cls
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

    findClassIdentifier :: HieASTs a -> Position -> MaybeT IO (Either ModuleName Name)
    findClassIdentifier ast instancePosition =
      pure
        $ head . head
        $ pointCommand ast instancePosition
          ( (Map.keys . Map.filter isClassNodeIdentifier . Compat.getNodeIds)
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

    findImplementedMethods :: HieASTs a -> Position -> MaybeT IO [T.Text]
    findImplementedMethods asts instancePosition = do
        pure
            $ concat
            $ pointCommand asts instancePosition
            $ map (T.pack . getOccString) . rights . findInstanceValBindIdentifiers

    -- | Recurses through the given AST to find identifiers which are
    -- 'InstanceValBind's.
    findInstanceValBindIdentifiers :: HieAST a -> [Identifier]
    findInstanceValBindIdentifiers ast =
        let valBindIds = Map.keys
                         . Map.filter (any isInstanceValBind . identInfo)
                         $ getNodeIds ast
        in valBindIds <> concatMap findInstanceValBindIdentifiers (nodeChildren ast)

ghostSpan :: RealSrcSpan
ghostSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<haskell-language-sever>") 1 1

containRange :: Range -> SrcSpan -> Bool
containRange range x = isInsideSrcSpan (range ^. J.start) x || isInsideSrcSpan (range ^. J.end) x

isClassNodeIdentifier :: IdentifierDetails a -> Bool
isClassNodeIdentifier ident = (isNothing . identType) ident && Use `Set.member` identInfo ident

isClassMethodWarning :: T.Text -> Bool
isClassMethodWarning = T.isPrefixOf "• No explicit implementation for"

isInstanceValBind :: ContextInfo -> Bool
isInstanceValBind (ValBind InstanceBind _ _) = True
isInstanceValBind _                          = False

minDefToMethodGroups :: BooleanFormula Name -> [[T.Text]]
minDefToMethodGroups = go
  where
    go (Var mn)   = [[T.pack . occNameString . occName $ mn]]
    go (Or ms)    = concatMap (go . unLoc) ms
    go (And ms)   = foldr (liftA2 (<>)) [[]] (fmap (go . unLoc) ms)
    go (Parens m) = go (unLoc m)
