{-# LANGUAGE CPP #-}
module Development.IDE.Plugin.Plugins.AddArgument (plugin) where

import           Control.Monad                             (join)
import           Control.Monad.Trans.Class                 (lift)
import           Data.Bifunctor                            (Bifunctor (..))
import           Data.Either.Extra                         (maybeToEither)
import qualified Data.Text                                 as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint     (exactPrint,
                                                            makeDeltaAst)
import           Development.IDE.GHC.Error                 (spanContainsRange)
import           Development.IDE.GHC.ExactPrint            (genAnchor1,
                                                            modifyMgMatchesT',
                                                            modifySigWithM,
                                                            modifySmallestDeclWithM)
import           Development.IDE.Plugin.Plugins.Diagnostic
import           GHC                                       (EpAnn (..),
                                                            SrcSpanAnnA,
                                                            SrcSpanAnnN,
                                                            emptyComments,
                                                            noAnn)
import           Ide.Plugin.Error                          (PluginError (PluginInternalError))
import           Ide.PluginUtils                           (makeDiffTextEdit)
import           Language.Haskell.GHC.ExactPrint           (TransformT (..),
                                                            noAnnSrcSpanDP1,
                                                            runTransformT)
import           Language.LSP.Protocol.Types

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,4,0)
import           GHC                                       (TrailingAnn (..))
import           GHC.Hs                                    (IsUnicodeSyntax (..))
import           Language.Haskell.GHC.ExactPrint.Transform (d1)
#endif

#if MIN_VERSION_ghc(9,4,0) && !MIN_VERSION_ghc(9,9,0)
import           Development.IDE.GHC.ExactPrint            (epl)
import           GHC.Parser.Annotation                     (TokenLocation (..))
#endif

#if !MIN_VERSION_ghc(9,9,0)
import           GHC                                       (SrcSpanAnn' (SrcSpanAnn))
import           GHC.Types.SrcLoc                          (generatedSrcSpan)
#endif

#if MIN_VERSION_ghc(9,9,0)
import           GHC                                       (EpUniToken (..))
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
addArgToMatch :: T.Text -> GenLocated l (Match GhcPs body) -> (GenLocated l (Match GhcPs body), Int)
addArgToMatch name (L locMatch (Match xMatch ctxMatch pats rhs)) =
  let unqualName = mkRdrUnqual $ mkVarOcc $ T.unpack name
#if MIN_VERSION_ghc(9,9,0)
      newPat = L noAnnSrcSpanDP1 $ VarPat NoExtField (noLocA unqualName)
#else
      newPat = L (noAnnSrcSpanDP1 generatedSrcSpan) $ VarPat NoExtField (noLocA unqualName)
#endif
  in (L locMatch (Match xMatch ctxMatch (pats <> [newPat]) rhs), Prelude.length pats)

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
addArgumentAction (ParsedModule _ moduleSrc _ _) range name _typ = do
    (newSource, _, _) <- runTransformT $ do
      (moduleSrc', join -> matchedDeclNameMay) <- addNameAsLastArgOfMatchingDecl (makeDeltaAst moduleSrc)
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
addTyHoleToTySigArg :: Int -> LHsSigType GhcPs -> (LHsSigType GhcPs)
addTyHoleToTySigArg loc (L annHsSig (HsSig xHsSig tyVarBndrs lsigTy)) =
    let (args, res) = hsTypeToFunTypeAsList lsigTy
#if MIN_VERSION_ghc(9,9,0)
        wildCardAnn = EpAnn genAnchor1 (AnnListItem []) emptyComments
        newArg = (noAnn, noExtField, HsUnrestrictedArrow NoEpUniTok, L wildCardAnn $ HsWildCardTy noExtField)
#elif MIN_VERSION_ghc(9,4,0)
        wildCardAnn = SrcSpanAnn (EpAnn genAnchor1 (AnnListItem []) emptyComments) generatedSrcSpan
        arrowAnn = TokenLoc (epl 1)
        newArg = (SrcSpanAnn mempty generatedSrcSpan, noAnn, HsUnrestrictedArrow (L arrowAnn HsNormalTok), L wildCardAnn $ HsWildCardTy noExtField)
#else
        wildCardAnn = SrcSpanAnn (EpAnn genAnchor1 (AnnListItem [AddRarrowAnn d1]) emptyComments) generatedSrcSpan
        newArg = (SrcSpanAnn mempty generatedSrcSpan, noAnn, HsUnrestrictedArrow NormalSyntax, L wildCardAnn $ HsWildCardTy noExtField)
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
