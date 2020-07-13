-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- ORIGINALLY COPIED FROM https://github.com/commercialhaskell/intero

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
#include "ghc-api-version.h"

-- | Get information on modules, identifiers, etc.

module Development.IDE.Spans.Calculate(getSrcSpanInfos) where

import           ConLike
import           Control.Monad
import qualified CoreUtils
import           Data.List
import           Data.Maybe
import           DataCon
import           Desugar
import           GhcMonad
import           HscTypes
import           FastString (mkFastString)
import           OccName
import           Development.IDE.Types.Location
import           Development.IDE.Spans.Type
import           Development.IDE.GHC.Error (zeroSpan, catchSrcErrors)
import           Prelude hiding (mod)
import           TcHsSyn
import           Var
import Development.IDE.Core.Compile
import qualified Development.IDE.GHC.Compat as Compat
import Development.IDE.GHC.Compat
import Development.IDE.GHC.Util
import Development.IDE.Spans.Common
import Development.IDE.Spans.Documentation
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map

-- A lot of things gained an extra X argument in GHC 8.6, which we mostly ignore
-- this U ignores that arg in 8.6, but is hidden in 8.4
#if MIN_GHC_API_VERSION(8,6,0)
#define U _
#else
#define U
#endif

-- | Get source span info, used for e.g. AtPoint and Goto Definition.
getSrcSpanInfos
    :: HscEnv
    -> [(Located ModuleName, Maybe NormalizedFilePath)] -- ^ Dependencies in topological order
    -> TcModuleResult
    -> [ParsedModule]   -- ^ Dependencies parsed, optional if the 'HscEnv' already contains docs
    -> IO SpansInfo
getSrcSpanInfos env imports tc parsedDeps =
    evalGhcEnv env $
        getSpanInfo imports tc parsedDeps

-- | Get ALL source spans in the module.
getSpanInfo :: GhcMonad m
            => [(Located ModuleName, Maybe NormalizedFilePath)] -- ^ imports
            -> TcModuleResult
            -> [ParsedModule]
            -> m SpansInfo
getSpanInfo mods TcModuleResult{tmrModInfo, tmrModule = tcm@TypecheckedModule{..}} parsedDeps =
  do let tcs = tm_typechecked_source
         bs  = listifyAllSpans  tcs :: [LHsBind GhcTc]
         es  = listifyAllSpans  tcs :: [LHsExpr GhcTc]
         ps  = listifyAllSpans' tcs :: [Pat GhcTc]
         ts  = listifyAllSpans tm_renamed_source :: [LHsType GhcRn]
         allModules = tm_parsed_module : parsedDeps
         funBinds = funBindMap tm_parsed_module
         thisMod = ms_mod $ pm_mod_summary tm_parsed_module
         modIface = hm_iface tmrModInfo

     -- Load this module in HPT to make its interface documentation available
     modifySession (loadModuleHome $ HomeModInfo modIface (snd tm_internals_) Nothing)

     bts <- mapM (getTypeLHsBind funBinds) bs   -- binds
     ets <- mapM getTypeLHsExpr es -- expressions
     pts <- mapM getTypeLPat  ps -- patterns
     tts <- concat <$> mapM getLHsType ts -- types

     -- Batch extraction of kinds
     let typeNames = nubOrd [ n | (Named n, _) <- tts]
     kinds <- Map.fromList . zip typeNames  <$> mapM (lookupKind thisMod) typeNames
     let withKind (Named n, x) =
            (Named n, x, join $ Map.lookup n kinds)
         withKind (other, x) =
            (other, x, Nothing)
     tts <- pure $ map withKind tts

     let imports = importInfo mods
     let exports = getExports tcm
     let exprs = addEmptyInfo exports ++ addEmptyInfo imports ++ concat bts ++ tts ++ catMaybes (ets ++ pts)
     let constraints = map constraintToInfo (concatMap getConstraintsLHsBind bs)
         sortedExprs = sortBy cmp exprs
         sortedConstraints = sortBy cmp constraints

    -- Batch extraction of Haddocks
     let names = nubOrd [ s | (Named s,_,_) <- sortedExprs ++ sortedConstraints]
     docs <- Map.fromList . zip names <$> getDocumentationsTryGhc thisMod allModules names
     let withDocs (Named n, x, y) = (Named n, x, y, Map.findWithDefault emptySpanDoc n docs)
         withDocs (other, x, y) = (other, x, y, emptySpanDoc)

     return $ SpansInfo (mapMaybe (toSpanInfo . withDocs) sortedExprs)
                        (mapMaybe (toSpanInfo . withDocs) sortedConstraints)
  where cmp (_,a,_) (_,b,_)
          | a `isSubspanOf` b = LT
          | b `isSubspanOf` a = GT
          | otherwise         = compare (srcSpanStart a) (srcSpanStart b)

        addEmptyInfo = map (\(a,b) -> (a,b,Nothing))
        constraintToInfo (sp, ty) = (SpanS sp, sp, Just ty)

lookupKind :: GhcMonad m => Module -> Name -> m (Maybe Type)
lookupKind mod =
    fmap (either (const Nothing) (safeTyThingType =<<)) . catchSrcErrors "span" . lookupName mod
-- | The locations in the typechecked module are slightly messed up in some cases (e.g. HsMatchContext always
-- points to the first match) whereas the parsed module has the correct locations.
-- Therefore we build up a map from OccName to the corresponding definition in the parsed module
-- to lookup precise locations for things like multi-clause function definitions.
--
-- For now this only contains FunBinds.
funBindMap :: ParsedModule -> OccEnv (HsBind GhcPs)
funBindMap pm = mkOccEnv $ [ (occName $ unLoc f, bnd) | L _ (Compat.ValD bnd@FunBind{fun_id = f}) <- hsmodDecls $ unLoc $ pm_parsed_source pm ]

getExports :: TypecheckedModule -> [(SpanSource, SrcSpan)]
getExports m
    | Just (_, _, Just exports, _) <- renamedSource m =
    [ (Named $ unLoc n, getLoc n)
    | (e, _) <- exports
    , n <- ieLNames $ unLoc e
    ]
getExports _ = []

-- | Variant of GHC's ieNames that produces LIdP instead of IdP
ieLNames :: IE pass -> [Located (IdP pass)]
ieLNames (IEVar       U n   )     = [ieLWrappedName n]
ieLNames (IEThingAbs  U n   )     = [ieLWrappedName n]
ieLNames (IEThingAll    n   )     = [ieLWrappedName n]
ieLNames (IEThingWith   n _ ns _) = ieLWrappedName n : map ieLWrappedName ns
ieLNames _ = []

-- | Get the name and type of a binding.
getTypeLHsBind :: (Monad m)
               => OccEnv (HsBind GhcPs)
               -> LHsBind GhcTc
               -> m [(SpanSource, SrcSpan, Maybe Type)]
getTypeLHsBind funBinds (L _spn FunBind{fun_id = pid})
  | Just FunBind {fun_matches = MG{mg_alts=L _ matches}} <- lookupOccEnv funBinds (occName $ unLoc pid) = do
  let name = getName (unLoc pid)
  return [(Named name, getLoc mc_fun, Just (varType (unLoc pid))) | match <- matches, FunRhs{mc_fun = mc_fun} <- [m_ctxt $ unLoc match] ]
-- In theory this shouldn’t ever fail but if it does, we can at least show the first clause.
getTypeLHsBind _ (L _spn FunBind{fun_id = pid,fun_matches = MG{}}) = do
  let name = getName (unLoc pid)
  return [(Named name, getLoc pid, Just (varType (unLoc pid)))]
getTypeLHsBind _ _ = return []

-- | Get information about constraints
getConstraintsLHsBind :: LHsBind GhcTc
                      -> [(SrcSpan, Type)]
getConstraintsLHsBind (L spn AbsBinds { abs_ev_vars = vars })
  = map (\v -> (spn, varType v)) vars
getConstraintsLHsBind _ = []

-- | Get the name and type of an expression.
getTypeLHsExpr :: (GhcMonad m)
               => LHsExpr GhcTc
               -> m (Maybe (SpanSource, SrcSpan, Maybe Type))
getTypeLHsExpr e = do
  hs_env <- getSession
  (_, mbe) <- liftIO (deSugarExpr hs_env e)
  case mbe of
    Just expr -> do
      let ss = getSpanSource (unLoc e)
      return $ Just (ss, getLoc e, Just (CoreUtils.exprType expr))
    Nothing -> return Nothing
  where
    getSpanSource :: HsExpr GhcTc -> SpanSource
    getSpanSource xpr | isLit xpr = Lit (showGhc xpr)
    getSpanSource (HsVar U (L _ i)) = Named (getName i)
    getSpanSource (HsConLikeOut U (RealDataCon dc)) = Named (dataConName dc)
    getSpanSource RecordCon {rcon_con_name} = Named (getName rcon_con_name)
    getSpanSource (HsWrap U _ xpr) = getSpanSource xpr
    getSpanSource (HsPar U xpr) = getSpanSource (unLoc xpr)
    getSpanSource _ = NoSource

    isLit :: HsExpr GhcTc -> Bool
    isLit (HsLit U _) = True
    isLit (HsOverLit U _) = True
    isLit (ExplicitTuple U args _) = all (isTupLit . unLoc) args
#if MIN_GHC_API_VERSION(8,6,0)
    isLit (ExplicitSum  U _ _ xpr) = isLitChild (unLoc xpr)
    isLit (ExplicitList U _ xprs) = all (isLitChild . unLoc) xprs
#else
    isLit (ExplicitSum  _ _ xpr _) = isLitChild (unLoc xpr)
    isLit (ExplicitList _ _ xprs) = all (isLitChild . unLoc) xprs
#endif
    isLit _ = False

    isTupLit (Present U xpr) = isLitChild (unLoc xpr)
    isTupLit _               = False

    -- We need special treatment for children so things like [(1)] are still treated
    -- as a list literal while not treating (1) as a literal.
    isLitChild (HsWrap U _ xpr) = isLitChild xpr
    isLitChild (HsPar U xpr)    = isLitChild (unLoc xpr)
#if MIN_GHC_API_VERSION(8,8,0)
    isLitChild (ExprWithTySig U xpr _) = isLitChild (unLoc xpr)
#elif MIN_GHC_API_VERSION(8,6,0)
    isLitChild (ExprWithTySig U xpr) = isLitChild (unLoc xpr)
#else
    isLitChild (ExprWithTySigOut xpr _) = isLitChild (unLoc xpr)
    isLitChild (ExprWithTySig xpr _) = isLitChild (unLoc xpr)
#endif
    isLitChild e = isLit e

-- | Get the name and type of a pattern.
getTypeLPat :: (Monad m)
            => Pat GhcTc
            -> m (Maybe (SpanSource, SrcSpan, Maybe Type))
getTypeLPat pat = do
  let (src, spn) = getSpanSource pat
  return $ Just (src, spn, Just (hsPatType pat))
  where
    getSpanSource :: Pat GhcTc -> (SpanSource, SrcSpan)
    getSpanSource (VarPat (L spn vid)) = (Named (getName vid), spn)
    getSpanSource (ConPatOut (L spn (RealDataCon dc)) _ _ _ _ _ _) =
      (Named (dataConName dc), spn)
    getSpanSource _ = (NoSource, noSrcSpan)

getLHsType
    :: Monad m
    => LHsType GhcRn
    -> m [(SpanSource, SrcSpan)]
getLHsType (L spn (HsTyVar U _ v)) = do
  let n = unLoc v
  pure [(Named n, spn)]
getLHsType _ = pure []

importInfo :: [(Located ModuleName, Maybe NormalizedFilePath)]
           -> [(SpanSource, SrcSpan)]
importInfo = mapMaybe (uncurry wrk) where
  wrk :: Located ModuleName -> Maybe NormalizedFilePath -> Maybe (SpanSource, SrcSpan)
  wrk modName = \case
    Nothing -> Nothing
    Just fp -> Just (fpToSpanSource $ fromNormalizedFilePath fp, getLoc modName)

  -- TODO make this point to the module name
  fpToSpanSource :: FilePath -> SpanSource
  fpToSpanSource fp = SpanS $ RealSrcSpan $ zeroSpan $ mkFastString fp

-- | Pretty print the types into a 'SpanInfo'.
toSpanInfo :: (SpanSource, SrcSpan, Maybe Type, SpanDoc) -> Maybe SpanInfo
toSpanInfo (name,mspan,typ,docs) =
  case mspan of
    RealSrcSpan spn ->
      -- GHC’s line and column numbers are 1-based while LSP’s line and column
      -- numbers are 0-based.
      Just (SpanInfo (srcSpanStartLine spn - 1)
                     (srcSpanStartCol spn - 1)
                     (srcSpanEndLine spn - 1)
                     (srcSpanEndCol spn - 1)
                     typ
                     name
                     docs)
    _ -> Nothing
