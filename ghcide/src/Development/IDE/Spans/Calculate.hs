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
import           GHC
import           GhcMonad
import           HscTypes
import           FastString (mkFastString)
import           OccName
import           Development.IDE.Types.Location
import           Development.IDE.Spans.Type
#ifdef GHC_LIB
import           Development.IDE.GHC.Error (zeroSpan)
#else
import           Development.IDE.GHC.Error (zeroSpan, catchSrcErrors)
#endif
import           Prelude hiding (mod)
import           TcHsSyn
import           Var
import Development.IDE.Core.Compile
import qualified Development.IDE.GHC.Compat as Compat
import Development.IDE.GHC.Util
import Development.IDE.Spans.Common
import Development.IDE.Spans.Documentation

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
    -> [ParsedModule]   -- ^ Dependencies parsed, optional
    -> [ModIface]       -- ^ Dependencies module interfaces, required
    -> IO SpansInfo
getSrcSpanInfos env imports tc parsedDeps deps =
    evalGhcEnv env $
        getSpanInfo imports (tmrModule tc) parsedDeps deps

-- | Get ALL source spans in the module.
getSpanInfo :: GhcMonad m
            => [(Located ModuleName, Maybe NormalizedFilePath)] -- ^ imports
            -> TypecheckedModule
            -> [ParsedModule]
            -> [ModIface]
            -> m SpansInfo
getSpanInfo mods tcm@TypecheckedModule{..} parsedDeps deps =
  do let tcs = tm_typechecked_source
         bs  = listifyAllSpans  tcs :: [LHsBind GhcTc]
         es  = listifyAllSpans  tcs :: [LHsExpr GhcTc]
         ps  = listifyAllSpans' tcs :: [Pat GhcTc]
         ts  = listifyAllSpans tm_renamed_source :: [LHsType GhcRn]
         allModules = tm_parsed_module : parsedDeps
         funBinds = funBindMap tm_parsed_module

     -- Load all modules in HPT to make their interface documentation available
     mapM_ (`loadDepModule` Nothing) (reverse deps)
     forM_ (modInfoIface tm_checked_module_info) $ \modIface ->
       modifySession (loadModuleHome $ HomeModInfo modIface (snd tm_internals_) Nothing)

     bts <- mapM (getTypeLHsBind allModules funBinds) bs   -- binds
     ets <- mapM (getTypeLHsExpr allModules) es -- expressions
     pts <- mapM (getTypeLPat allModules)    ps -- patterns
     tts <- mapM (getLHsType allModules)     ts -- types
     let imports = importInfo mods
     let exports = getExports tcm
     let exprs = addEmptyInfo exports ++ addEmptyInfo imports ++ concat bts ++ concat tts ++ catMaybes (ets ++ pts)
     let constraints = map constraintToInfo (concatMap getConstraintsLHsBind bs)
     return $ SpansInfo (mapMaybe toSpanInfo (sortBy cmp exprs))
                        (mapMaybe toSpanInfo (sortBy cmp constraints))
  where cmp (_,a,_,_) (_,b,_,_)
          | a `isSubspanOf` b = LT
          | b `isSubspanOf` a = GT
          | otherwise         = compare (srcSpanStart a) (srcSpanStart b)

        addEmptyInfo = map (\(a,b) -> (a,b,Nothing,emptySpanDoc))
        constraintToInfo (sp, ty) = (SpanS sp, sp, Just ty, emptySpanDoc)

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
ieLNames (IEThingAll  U n   )     = [ieLWrappedName n]
ieLNames (IEThingWith U n _ ns _) = ieLWrappedName n : map ieLWrappedName ns
ieLNames _ = []

-- | Get the name and type of a binding.
getTypeLHsBind :: (GhcMonad m)
               => [ParsedModule]
               -> OccEnv (HsBind GhcPs)
               -> LHsBind GhcTc
               -> m [(SpanSource, SrcSpan, Maybe Type, SpanDoc)]
getTypeLHsBind deps funBinds (L _spn FunBind{fun_id = pid})
  | Just FunBind {fun_matches = MG{mg_alts=L _ matches}} <- lookupOccEnv funBinds (occName $ unLoc pid) = do
  let name = getName (unLoc pid)
  docs <- getDocumentationTryGhc deps name
  return [(Named name, getLoc mc_fun, Just (varType (unLoc pid)), docs) | match <- matches, FunRhs{mc_fun = mc_fun} <- [m_ctxt $ unLoc match] ]
-- In theory this shouldn’t ever fail but if it does, we can at least show the first clause.
getTypeLHsBind deps _ (L _spn FunBind{fun_id = pid,fun_matches = MG{}}) = do
  let name = getName (unLoc pid)
  docs <- getDocumentationTryGhc deps name
  return [(Named name, getLoc pid, Just (varType (unLoc pid)), docs)]
getTypeLHsBind _ _ _ = return []

-- | Get information about constraints
getConstraintsLHsBind :: LHsBind GhcTc
                      -> [(SrcSpan, Type)]
getConstraintsLHsBind (L spn AbsBinds { abs_ev_vars = vars })
  = map (\v -> (spn, varType v)) vars
getConstraintsLHsBind _ = []

-- | Get the name and type of an expression.
getTypeLHsExpr :: (GhcMonad m)
               => [ParsedModule]
               -> LHsExpr GhcTc
               -> m (Maybe (SpanSource, SrcSpan, Maybe Type, SpanDoc))
getTypeLHsExpr deps e = do
  hs_env <- getSession
  (_, mbe) <- liftIO (deSugarExpr hs_env e)
  case mbe of
    Just expr -> do
      let ss = getSpanSource (unLoc e)
      docs <- case ss of
                Named n -> getDocumentationTryGhc deps n
                _       -> return emptySpanDoc
      return $ Just (ss, getLoc e, Just (CoreUtils.exprType expr), docs)
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
getTypeLPat :: (GhcMonad m)
            => [ParsedModule]
            -> Pat GhcTc
            -> m (Maybe (SpanSource, SrcSpan, Maybe Type, SpanDoc))
getTypeLPat deps pat = do
  let (src, spn) = getSpanSource pat
  docs <- case src of
            Named n -> getDocumentationTryGhc deps n
            _       -> return emptySpanDoc
  return $ Just (src, spn, Just (hsPatType pat), docs)
  where
    getSpanSource :: Pat GhcTc -> (SpanSource, SrcSpan)
    getSpanSource (VarPat U (L spn vid)) = (Named (getName vid), spn)
    getSpanSource (ConPatOut (L spn (RealDataCon dc)) _ _ _ _ _ _) =
      (Named (dataConName dc), spn)
    getSpanSource _ = (NoSource, noSrcSpan)

getLHsType
    :: GhcMonad m
    => [ParsedModule]
    -> LHsType GhcRn
    -> m [(SpanSource, SrcSpan, Maybe Type, SpanDoc)]
getLHsType deps (L spn (HsTyVar U _ v)) = do
  let n = unLoc v
  docs <- getDocumentationTryGhc deps n
#ifdef GHC_LIB
  let ty = Right Nothing
#else
  ty <- catchSrcErrors "completion" $ do
          name' <- lookupName n
          return $ name' >>= safeTyThingType
#endif
  let ty' = case ty of
              Right (Just x) -> Just x
              _ -> Nothing
  pure [(Named n, spn, ty', docs)]
getLHsType _ _ = pure []

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
