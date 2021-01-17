{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Development.IDE.Plugin.CodeAction.ExactPrint
  ( Rewrite (..),
    rewriteToEdit,

    -- * Utilities
    appendConstraint,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Data (Data)
import Data.Functor
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Development.IDE.GHC.Compat hiding (parseExpr)
import Development.IDE.GHC.ExactPrint
import Development.IDE.Types.Location
import GhcPlugins (realSrcSpanEnd, realSrcSpanStart, sigPrec)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (DP), KeywordId (G), mkAnnKey)
import Language.Haskell.LSP.Types

------------------------------------------------------------------------------

-- | Construct a 'Rewrite', replacing the node at the given 'SrcSpan' with the
--   given 'ast'.
data Rewrite where
  Rewrite ::
    Annotate ast =>
    -- | The 'SrcSpan' that we want to rewrite
    SrcSpan ->
    -- | The ast that we want to graft
    (DynFlags -> TransformT (Either String) (Located ast)) ->
    Rewrite

------------------------------------------------------------------------------

-- | Convert a 'Rewrite' into a 'WorkspaceEdit'.
rewriteToEdit ::
  DynFlags ->
  Uri ->
  Anns ->
  Rewrite ->
  Either String WorkspaceEdit
rewriteToEdit dflags uri anns (Rewrite dst f) = do
  (ast, (anns, _), _) <- runTransformT anns $ f dflags
  let editMap =
        HMap.fromList
          [ ( uri,
              List
                [ TextEdit (fromJust $ srcSpanToRange dst) $
                      T.pack $ tail $ exactPrint ast anns
                ]
            )
          ]
  pure $ WorkspaceEdit (Just editMap) Nothing

srcSpanToRange :: SrcSpan -> Maybe Range
srcSpanToRange (UnhelpfulSpan _) = Nothing
srcSpanToRange (RealSrcSpan real) = Just $ realSrcSpanToRange real

realSrcSpanToRange :: RealSrcSpan -> Range
realSrcSpanToRange real =
  Range
    (realSrcLocToPosition $ realSrcSpanStart real)
    (realSrcLocToPosition $ realSrcSpanEnd real)

realSrcLocToPosition :: RealSrcLoc -> Position
realSrcLocToPosition real =
  Position (srcLocLine real - 1) (srcLocCol real - 1)

------------------------------------------------------------------------------

-- | Fix the parentheses around a type context
fixParens ::
  (Monad m, Data (HsType pass)) =>
  Maybe DeltaPos ->
  Maybe DeltaPos ->
  LHsContext pass ->
  TransformT m [LHsType pass]
fixParens openDP closeDP ctxt@(L _ elems) = do
  -- Paren annotation for type contexts are usually quite screwed up
  -- we remove duplicates and fix negative DPs
  modifyAnnsT $
    Map.adjust
      ( \x ->
          let annsMap = Map.fromList (annsDP x)
           in x
                { annsDP =
                    Map.toList $
                      Map.alter (\_ -> openDP <|> Just dp00) (G AnnOpenP) $
                        Map.alter (\_ -> closeDP <|> Just dp00) (G AnnCloseP) $
                          annsMap <> parens
                }
      )
      (mkAnnKey ctxt)
  return $ map dropHsParTy elems
  where
    parens = Map.fromList [(G AnnOpenP, dp00), (G AnnCloseP, dp00)]

    dropHsParTy :: LHsType pass -> LHsType pass
    dropHsParTy (L _ (HsParTy _ ty)) = ty
    dropHsParTy other = other

-- | Append a constraint at the end of a type context.
--   If no context is present, a new one will be created.
appendConstraint ::
  -- | The new constraint to append
  String ->
  -- | The type signature where the constraint is to be inserted, also assuming annotated
  LHsType GhcPs ->
  Rewrite
appendConstraint constraintT = go
  where
    go (L l it@HsQualTy {hst_ctxt = L l' ctxt}) = Rewrite l $ \df -> do
      constraint <- liftParseAST df constraintT
      setEntryDPT constraint (DP (0, 1))

      -- Paren annotations are usually attached to the first and last constraints,
      -- rather than to the constraint list itself, so to preserve them we need to reposition them
      closeParenDP <- lookupAnn (G AnnCloseP) `mapM` lastMaybe ctxt
      openParenDP <- lookupAnn (G AnnOpenP) `mapM` headMaybe ctxt
      ctxt' <- fixParens (join openParenDP) (join closeParenDP) (L l' ctxt)

      addTrailingCommaT (last ctxt')

      return $ L l $ it {hst_ctxt = L l' $ ctxt' ++ [constraint]}
    go (L _ HsForAllTy {hst_body}) = go hst_body
    go (L _ (HsParTy _ ty)) = go ty
    go (L l other) = Rewrite l $ \df -> do
      -- there isn't a context, so we must create one
      constraint <- liftParseAST df constraintT
      lContext <- uniqueSrcSpanT
      lTop <- uniqueSrcSpanT
      let context = L lContext [constraint]
      addSimpleAnnT context (DP (0, 1)) $
        [ (G AnnDarrow, DP (0, 1))
        ]
          ++ concat
            [ [ (G AnnOpenP, dp00),
                (G AnnCloseP, dp00)
              ]
              | hsTypeNeedsParens sigPrec $ unLoc constraint
            ]
      return $ L lTop $ HsQualTy noExtField context (L l other)

liftParseAST :: ASTElement ast => DynFlags -> String -> TransformT (Either String) (Located ast)
liftParseAST df s = case parseAST df "" s of
  Right (anns, x) -> modifyAnnsT (anns <>) $> x
  Left _ -> lift $ Left $ "No parse: " <> s

lookupAnn :: (Data a, Monad m) => KeywordId -> Located a -> TransformT m (Maybe DeltaPos)
lookupAnn comment la = do
  anns <- getAnnsT
  return $ Map.lookup (mkAnnKey la) anns >>= lookup comment . annsDP

dp00 :: DeltaPos
dp00 = DP (0, 0)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (a : _) = Just a

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe other = Just $ last other
