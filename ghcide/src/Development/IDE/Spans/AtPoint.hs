-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | Gives information about symbols at a given point in DAML files.
-- These are all pure functions that should execute quickly.
module Development.IDE.Spans.AtPoint (
    atPoint
  , gotoDefinition
  , gotoTypeDefinition
  , documentHighlight
  , pointCommand
  ) where

import           Development.IDE.GHC.Error
import Development.IDE.GHC.Orphans()
import Development.IDE.Types.Location
import           Language.Haskell.LSP.Types

-- DAML compiler and infrastructure
import Development.IDE.GHC.Compat
import Development.IDE.Types.Options
import Development.IDE.Spans.Common
import Development.IDE.Core.RuleTypes

-- GHC API imports
import FastString
import Name
import Outputable hiding ((<>))
import SrcLoc
import TyCoRep
import TyCon
import qualified Var
import NameEnv

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import           Data.Maybe
import           Data.List
import qualified Data.Text as T
import qualified Data.Map as M

import Data.Either
import Data.List.Extra (dropEnd1)

documentHighlight
  :: Monad m
  => HieASTs Type
  -> RefMap
  -> Position
  -> MaybeT m [DocumentHighlight]
documentHighlight hf rf pos = MaybeT $ pure (Just highlights)
  where
    ns = concat $ pointCommand hf pos (rights . M.keys . nodeIdentifiers . nodeInfo)
    highlights = do
      n <- ns
      ref <- maybe [] id (M.lookup (Right n) rf)
      pure $ makeHighlight ref
    makeHighlight (sp,dets) =
      DocumentHighlight (realSrcSpanToRange sp) (Just $ highlightType $ identInfo dets)
    highlightType s =
      if any (isJust . getScopeFromContext) s
        then HkWrite
        else HkRead

gotoTypeDefinition
  :: MonadIO m
  => (Module -> MaybeT m (HieFile, FilePath))
  -> IdeOptions
  -> HieASTs Type
  -> Position
  -> MaybeT m [Location]
gotoTypeDefinition getHieFile ideOpts srcSpans pos
  = lift $ typeLocationsAtPoint getHieFile ideOpts pos srcSpans

-- | Locate the definition of the name at a given position.
gotoDefinition
  :: MonadIO m
  => (Module -> MaybeT m (HieFile, FilePath))
  -> IdeOptions
  -> M.Map ModuleName NormalizedFilePath
  -> HieASTs Type
  -> Position
  -> MaybeT m Location
gotoDefinition getHieFile ideOpts imports srcSpans pos
  = MaybeT $ fmap listToMaybe $ locationsAtPoint getHieFile ideOpts imports pos srcSpans

-- | Synopsis for the name at a given position.
atPoint
  :: IdeOptions
  -> HieASTs Type
  -> DocAndKindMap
  -> Position
  -> Maybe (Maybe Range, [T.Text])
atPoint IdeOptions{} hf (DKMap dm km) pos = listToMaybe $ pointCommand hf pos hoverInfo
  where
    -- Hover info for values/data
    hoverInfo ast =
      (Just range, prettyNames ++ pTypes)
      where
        pTypes
          | length names == 1 = dropEnd1 $ map wrapHaskell prettyTypes
          | otherwise = map wrapHaskell prettyTypes

        range = realSrcSpanToRange $ nodeSpan ast

        wrapHaskell x = "\n```haskell\n"<>x<>"\n```\n"
        info = nodeInfo ast
        names = M.assocs $ nodeIdentifiers info
        types = nodeType info

        prettyNames :: [T.Text]
        prettyNames = map prettyName names
        prettyName (Right n, dets) = T.unlines $
          wrapHaskell (showNameWithoutUniques n <> maybe "" ((" :: " <>) . prettyType) (identType dets <|> maybeKind))
          : definedAt n
          ++ catMaybes [ T.unlines . spanDocToMarkdown <$> lookupNameEnv dm n
                      ]
          where maybeKind = safeTyThingType =<< lookupNameEnv km n
        prettyName (Left m,_) = showName m

        prettyTypes = map (("_ :: "<>) . prettyType) types
        prettyType t = showName t

        definedAt name =
          -- do not show "at <no location info>" and similar messages
          -- see the code of 'pprNameDefnLoc' for more information
          case nameSrcLoc name of
            UnhelpfulLoc {} | isInternalName name || isSystemName name -> []
            _ -> ["*Defined " <> T.pack (showSDocUnsafe $ pprNameDefnLoc name) <> "*"]

typeLocationsAtPoint
  :: forall m
   . MonadIO m
  => (Module -> MaybeT m (HieFile, FilePath))
  -> IdeOptions
  -> Position
  -> HieASTs Type
  -> m [Location]
typeLocationsAtPoint getHieFile _ideOptions pos ast =
  let ts = concat $ pointCommand ast pos (nodeType . nodeInfo)
      ns = flip mapMaybe ts $ \case
        TyConApp tc _ -> Just $ tyConName tc
        TyVarTy n -> Just $ Var.varName n
        _ -> Nothing
    in mapMaybeM (nameToLocation getHieFile) ns

locationsAtPoint
  :: forall m
   . MonadIO m
  => (Module -> MaybeT m (HieFile, FilePath))
  -> IdeOptions
  -> M.Map ModuleName NormalizedFilePath
  -> Position
  -> HieASTs Type
  -> m [Location]
locationsAtPoint getHieFile _ideOptions imports pos ast =
  let ns = concat $ pointCommand ast pos (M.keys . nodeIdentifiers . nodeInfo)
      zeroPos = Position 0 0
      zeroRange = Range zeroPos zeroPos
      modToLocation m = fmap (\fs -> Location (fromNormalizedUri $ filePathToUri' fs) zeroRange) $ M.lookup m imports
    in mapMaybeM (either (pure . modToLocation) $ nameToLocation getHieFile) ns

-- | Given a 'Name' attempt to find the location where it is defined.
nameToLocation :: Monad f => (Module -> MaybeT f (HieFile, String)) -> Name -> f (Maybe Location)
nameToLocation getHieFile name = fmap (srcSpanToLocation =<<) $
  case nameSrcSpan name of
    sp@(RealSrcSpan _) -> pure $ Just sp
    sp@(UnhelpfulSpan _) -> runMaybeT $ do
      guard (sp /= wiredInSrcSpan)
      -- This case usually arises when the definition is in an external package.
      -- In this case the interface files contain garbage source spans
      -- so we instead read the .hie files to get useful source spans.
      mod <- MaybeT $ return $ nameModule_maybe name
      (hieFile, srcPath) <- getHieFile mod
      avail <- MaybeT $ pure $ find (eqName name . snd) $ hieExportNames hieFile
      -- The location will point to the source file used during compilation.
      -- This file might no longer exists and even if it does the path will be relative
      -- to the compilation directory which we don’t know.
      let span = setFileName srcPath $ fst avail
      pure span
  where
    -- We ignore uniques and source spans and only compare the name and the module.
    eqName :: Name -> Name -> Bool
    eqName n n' = nameOccName n == nameOccName n' && nameModule_maybe n == nameModule_maybe n'
    setFileName f (RealSrcSpan span) = RealSrcSpan (span { srcSpanFile = mkFastString f })
    setFileName _ span@(UnhelpfulSpan _) = span

pointCommand :: HieASTs Type -> Position -> (HieAST Type -> a) -> [a]
pointCommand hf pos k =
    catMaybes $ M.elems $ flip M.mapWithKey (getAsts hf) $ \fs ast ->
      case selectSmallestContaining (sp fs) ast of
        Nothing -> Nothing
        Just ast' -> Just $ k ast'
 where
   sloc fs = mkRealSrcLoc fs (line+1) (cha+1)
   sp fs = mkRealSrcSpan (sloc fs) (sloc fs)
   line = _line pos
   cha = _character pos


