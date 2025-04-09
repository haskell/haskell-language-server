{-# LANGUAGE CPP #-}
module Development.IDE.Plugin.Plugins.AddArgument (plugin) where

import           Control.Monad                             (join)
import           Control.Monad.Trans.Class                 (lift)
import           Data.Bifunctor                            (Bifunctor (..))
import           Data.Either.Extra                         (maybeToEither)
import qualified Data.Text                                 as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error                 (spanContainsRange)
import           Development.IDE.GHC.ExactPrint            (modifyMgMatchesT',
                                                            modifySigWithM,
                                                            modifySmallestDeclWithM)
import           Development.IDE.Plugin.Plugins.Diagnostic
import           GHC.Parser.Annotation                     (SrcSpanAnnA,
                                                            SrcSpanAnnN, noAnn)
import           Ide.Plugin.Error                          (PluginError (PluginInternalError))
import           Ide.PluginUtils                           (makeDiffTextEdit)
import           Language.Haskell.GHC.ExactPrint           (TransformT (..),
                                                            exactPrint,
                                                            noAnnSrcSpanDP1,
                                                            runTransformT)
import           Language.LSP.Protocol.Types

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,4,0)
import           GHC.Parser.Annotation                     (IsUnicodeSyntax (..),
                                                            TrailingAnn (..))
import           Language.Haskell.GHC.ExactPrint           (d1)
#endif

#if MIN_VERSION_ghc(9,4,0) && !MIN_VERSION_ghc(9,9,0)
import           Development.IDE.GHC.ExactPrint            (epl)
import           GHC.Parser.Annotation                     (TokenLocation (..))
#endif

#if !MIN_VERSION_ghc(9,9,0)
import           Development.IDE.GHC.Compat.ExactPrint     (makeDeltaAst)
import           Development.IDE.GHC.ExactPrint            (genAnchor1)
import           GHC.Parser.Annotation                     (EpAnn (..),
                                                            SrcSpanAnn' (..),
                                                            emptyComments)
import           GHC.Types.SrcLoc                          (generatedSrcSpan)
#endif

#if MIN_VERSION_ghc(9,9,0)
import           GHC                                       (DeltaPos (..),
                                                            EpUniToken (..),
                                                            IsUnicodeSyntax (NormalSyntax))
import           Language.Haskell.GHC.ExactPrint           (d1, setEntryDP)
#endif
#if MIN_VERSION_ghc(9,11,0)
import GHC.Parser.Annotation (EpToken(..))
#endif

-- When GHC tells us that a variable is not bound, it will tell us either:
--  - there is an unbound variable with a given type
--  - there is an unbound variable (GHC provides no type suggestion)
--
-- When we receive either of these errors, we produce a text edit that will add a new argument (as a new pattern in the
-- last position of each LHS of the top-level bindings for this HsDecl).
--
-- NOTE When adding a new argument to a declaration, the corresponding argument's type in declaration's signature might
--      not be the last type in the signature, such as:
--         foo :: a -> b -> c -> d
--         foo a b = \c -> ...
--      In this case a new argument would have to add its type between b and c in the signature.
plugin :: ParsedModule -> Diagnostic -> Either PluginError [(T.Text, [TextEdit])]
plugin parsedModule Diagnostic {_message, _range}
  | Just (name, typ) <- matchVariableNotInScope message = addArgumentAction parsedModule _range name typ
  | Just (name, typ) <- matchFoundHoleIncludeUnderscore message = addArgumentAction parsedModule _range name (Just typ)
  | otherwise = pure []
  where
    message = unifySpaces _message

-- Given a name for the new binding, add a new pattern to the match in the last position,
-- returning how many patterns there were in this match prior to the transformation:
--      addArgToMatch "foo" `bar arg1 arg2 = ...`
--   => (`bar arg1 arg2 foo = ...`, 2)
addArgToMatch :: T.Text -> GenLocated l (Match GhcPs (LocatedA (HsExpr GhcPs))) -> (GenLocated l (Match GhcPs (LocatedA (HsExpr GhcPs))), Int)
#if MIN_VERSION_ghc(9,11,0)
addArgToMatch name (L locMatch (Match xMatch ctxMatch (L l pats) rhs)) =
#else
addArgToMatch name (L locMatch (Match xMatch ctxMatch pats rhs)) =
#endif
#if MIN_VERSION_ghc(9,9,0)
  let unqualName = mkRdrUnqual $ mkVarOcc $ T.unpack name
      newPat = L noAnnSrcSpanDP1 $ VarPat NoExtField $ L noAnn unqualName
      -- The intention is to move `= ...` (right-hand side with equals) to the right so there's 1 space between
      -- the newly added pattern and the rest
      indentRhs :: GRHSs GhcPs (LocatedA (HsExpr GhcPs)) -> GRHSs GhcPs (LocatedA (HsExpr GhcPs))
      indentRhs rhs@GRHSs{grhssGRHSs} = rhs {grhssGRHSs = fmap (`setEntryDP` (SameLine 1)) grhssGRHSs }
#else
  let unqualName = mkRdrUnqual $ mkVarOcc $ T.unpack name
      newPat = L (noAnnSrcSpanDP1 generatedSrcSpan) $ VarPat NoExtField (noLocA unqualName)
      indentRhs = id
#endif
#if MIN_VERSION_ghc(9,11,0)
  in (L locMatch (Match xMatch ctxMatch (L l (pats <> [newPat])) (indentRhs rhs)), Prelude.length pats)
#else
  in (L locMatch (Match xMatch ctxMatch (pats <> [newPat]) (indentRhs rhs)), Prelude.length pats)
#endif

-- Attempt to insert a binding pattern into each match for the given LHsDecl; succeeds only if the function is a FunBind.
-- Also return:
--   - the declaration's name
--   - the number of bound patterns in the declaration's matches prior to the transformation
--
-- For example:
--    insertArg "new_pat" `foo bar baz = 1`
-- => (`foo bar baz new_pat = 1`, Just ("foo", 2))
appendFinalPatToMatches :: T.Text -> LHsDecl GhcPs -> TransformT (Either PluginError) (LHsDecl GhcPs, Maybe (GenLocated SrcSpanAnnN RdrName, Int))
appendFinalPatToMatches name = \case
  (L locDecl (ValD xVal fun@FunBind{fun_matches=mg,fun_id = idFunBind})) -> do
    (mg', numPatsMay) <- modifyMgMatchesT' mg (pure . second Just . addArgToMatch name) Nothing combineMatchNumPats
    numPats <- TransformT $ lift $ maybeToEither (PluginInternalError "Unexpected empty match group in HsDecl") numPatsMay
    let decl' = L locDecl (ValD xVal fun{fun_matches=mg'})
    pure (decl', Just (idFunBind, numPats))
  decl -> pure (decl, Nothing)
  where
    combineMatchNumPats  Nothing other = pure other
    combineMatchNumPats  other Nothing = pure other
    combineMatchNumPats  (Just l) (Just r)
      | l == r = pure (Just l)
      | otherwise = Left $ PluginInternalError "Unexpected different numbers of patterns in HsDecl MatchGroup"

-- The add argument works as follows:
--  1. Attempt to add the given name as the last pattern of the declaration that contains `range`.
--  2. If such a declaration exists, use that declaration's name to modify the signature of said declaration, if it
--     has a type signature.
--
-- NOTE For the following situation, the type signature is not updated (it's unclear what should happen):
--   type FunctionTySyn = () -> Int
--   foo :: FunctionTySyn
--   foo () = new_def
--
-- TODO instead of inserting a typed hole; use GHC's suggested type from the error
addArgumentAction :: ParsedModule -> Range -> T.Text -> Maybe T.Text -> Either PluginError [(T.Text, [TextEdit])]
addArgumentAction (ParsedModule _ moduleSrc _) range name _typ = do
    (newSource, _, _) <- runTransformT $ do
      (moduleSrc', join -> matchedDeclNameMay) <- addNameAsLastArgOfMatchingDecl
#if MIN_VERSION_ghc(9,9,0)
        moduleSrc
#else
        (makeDeltaAst moduleSrc)
#endif
      case matchedDeclNameMay of
          Just (matchedDeclName, numPats) -> modifySigWithM (unLoc matchedDeclName) (addTyHoleToTySigArg numPats) moduleSrc'
          Nothing -> pure moduleSrc'
    let diff = makeDiffTextEdit (T.pack $ exactPrint moduleSrc) (T.pack $ exactPrint newSource)
    pure [("Add argument ‘" <> name <> "’ to function", diff)]
  where
    addNameAsLastArgOfMatchingDecl = modifySmallestDeclWithM spanContainsRangeOrErr addNameAsLastArg
    addNameAsLastArg = fmap (first (:[])) . appendFinalPatToMatches name

    spanContainsRangeOrErr = maybeToEither (PluginInternalError "SrcSpan was not valid range") . (`spanContainsRange` range)

-- Transform an LHsType into a list of arguments and return type, to make transformations easier.
hsTypeToFunTypeAsList :: LHsType GhcPs -> ([(SrcSpanAnnA, XFunTy GhcPs, HsArrow GhcPs, LHsType GhcPs)], LHsType GhcPs)
hsTypeToFunTypeAsList = \case
  L spanAnnA (HsFunTy xFunTy arrow lhs rhs) ->
    let (rhsArgs, rhsRes) = hsTypeToFunTypeAsList rhs
    in ((spanAnnA, xFunTy, arrow, lhs):rhsArgs, rhsRes)
  ty -> ([], ty)

-- The inverse of `hsTypeToFunTypeAsList`
hsTypeFromFunTypeAsList :: ([(SrcSpanAnnA, XFunTy GhcPs, HsArrow GhcPs, LHsType GhcPs)], LHsType GhcPs) -> LHsType GhcPs
hsTypeFromFunTypeAsList (args, res) =
  foldr (\(spanAnnA, xFunTy, arrow, argTy) res -> L spanAnnA $ HsFunTy xFunTy arrow argTy res) res args

-- Add a typed hole to a type signature in the given argument position:
--   0 `foo :: ()` => foo :: _ -> ()
--   2 `foo :: FunctionTySyn` => foo :: FunctionTySyn
--   1 `foo :: () -> () -> Int` => foo :: () -> _ -> () -> Int
addTyHoleToTySigArg :: Int -> LHsSigType GhcPs -> LHsSigType GhcPs
addTyHoleToTySigArg loc (L annHsSig (HsSig xHsSig tyVarBndrs lsigTy)) =
    let (args, res) = hsTypeToFunTypeAsList lsigTy
#if MIN_VERSION_ghc(9,9,0)
        wildCardAnn = noAnnSrcSpanDP1
        newArg =
          ( noAnn
          , noExtField
          , HsUnrestrictedArrow (EpUniTok d1 NormalSyntax)
#if MIN_VERSION_ghc(9,11,0)
          , L wildCardAnn $ HsWildCardTy NoEpTok
#else
          , L wildCardAnn $ HsWildCardTy noExtField
#endif 
          )
#elif MIN_VERSION_ghc(9,4,0)
        wildCardAnn = SrcSpanAnn (EpAnn genAnchor1 (AnnListItem []) emptyComments) generatedSrcSpan
        arrowAnn = TokenLoc (epl 1)
        newArg =
          ( SrcSpanAnn mempty generatedSrcSpan
          , noAnn
          , HsUnrestrictedArrow (L arrowAnn HsNormalTok)
          , L wildCardAnn $ HsWildCardTy noExtField
          )
#else
        wildCardAnn = SrcSpanAnn (EpAnn genAnchor1 (AnnListItem [AddRarrowAnn d1]) emptyComments) generatedSrcSpan
        newArg =
          ( SrcSpanAnn mempty generatedSrcSpan
          , noAnn
          , HsUnrestrictedArrow NormalSyntax
          , L wildCardAnn $ HsWildCardTy noExtField
          )
#endif
        -- NOTE if the location that the argument wants to be placed at is not one more than the number of arguments
        --      in the signature, then we return the original type signature.
        --      This situation most likely occurs due to a function type synonym in the signature
        insertArg n _ | n < 0 = error "Not possible"
        insertArg 0 as = newArg:as
        insertArg _ [] = []
        insertArg n (a:as) = a : insertArg (n - 1) as
        lsigTy' = hsTypeFromFunTypeAsList (insertArg loc args, res)
    in L annHsSig (HsSig xHsSig tyVarBndrs lsigTy')
