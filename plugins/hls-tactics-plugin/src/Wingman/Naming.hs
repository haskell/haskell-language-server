module Wingman.Naming where

import           ConLike
import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Bool (bool)
import           Data.Char
import           Data.List (isPrefixOf)
import           Data.List.Extra (split)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Traversable
import           FieldLabel
import           GhcPlugins (unpackFS, charTy, maybeTyCon)
import           Name
import           TcType
import           Text.Hyphenation (hyphenate, english_US)
import           TyCon
import           Type
import           TysWiredIn (listTyCon, pairTyCon, unitTyCon)


fieldNames :: ConLike -> [OccName]
fieldNames cl =
  case fmap (unpackFS . flLabel) $ conLikeFieldLabels cl of
    [] -> []
    [_] -> []
    fields ->
      let field_first_segs = fmap (listToMaybe . split (== '_')) fields
      in case and $ zipWith (==) field_first_segs $ tail field_first_segs of
            True ->
              let common_prefix = maybe 0 ((+ 1) . length) $ head field_first_segs
               in fmap (mkVarOcc . drop common_prefix) fields
            False -> []

------------------------------------------------------------------------------
-- | Use type information to create a reasonable name.
mkTyName :: Type -> [String]
-- eg. mkTyName (a -> b) = "fab"
mkTyName (tcSplitFunTys -> ([a@(isFunTy -> False)], b))
  | isTyVarTy a && isTyVarTy b
  = (\x y z -> x <> y <> z) <$> ["f", "g", "h"] <*> mkTyName a <*> mkTyName b
-- eg. mkTyName (a -> Bool) = "p"
mkTyName (tcSplitFunTys -> ([isFunTy -> False], isBoolTy -> True))
  = pure $ "p"
-- eg. mkTyName (A -> B) = "f"
mkTyName (tcSplitFunTys -> ([isFunTy -> False], _))
  = ["f", "g", "h"]
-- eg. mkTyName (a -> b -> C) = "f_C"
mkTyName (tcSplitFunTys -> (_:_, b))
  = fmap ("f_" <>) $ mkTyName b
-- eg. mkTyName [Char] = "str"
mkTyName (splitTyConApp_maybe -> Just (c, [arg]))
  | c == listTyCon, eqType arg charTy
  = pure $ "str"
mkTyName (splitTyConApp_maybe -> Just (c, [arg]))
  | c == listTyCon, eqType arg charTy
  = pure $ "str"
-- eg. mkTyName Int = "n"
mkTyName (isIntTy -> True) = ["n", "i", "j"]
-- eg. mkTyName Integer = "n"
mkTyName (isIntegerTy -> True) = ["n", "i", "j"]
-- eg. mkTyName (T A B) = "tab"
mkTyName (splitTyConApp_maybe -> Just (c, args))
  = fmap (mkTyConName c $) $ foldMap mkTyName args
-- eg. mkTyName (f a) = "fa"
mkTyName (tcSplitAppTys -> (t, args@(_:_)))
  = liftA2 (<>) (mkTyName t) $ foldMap mkTyName args
-- eg. mkTyName a = "a"
mkTyName (getTyVar_maybe -> Just tv)
  = pure $ occNameString $ occName tv
-- eg. mkTyName (forall x. y) = "y"
mkTyName (tcSplitSigmaTy -> (_:_, _, t))
  = mkTyName t
mkTyName _ = pure $ "x"


------------------------------------------------------------------------------
-- | Get a good name for a type constructor.
mkTyConName :: TyCon -> String -> String
mkTyConName tc
  | tc == listTyCon = flip mappend "s"
  | tc == pairTyCon = mappend "p_"
  | tc == unitTyCon = mappend "unit"
  | tc == maybeTyCon = mappend "m_"
  | isSymOcc (getOccName tc)
      = mappend
      . take 1
      . fmap toLower
      . filterReplace isSymbol      's'
      . filterReplace isPunctuation 'p'
      . occNameString
      $ getOccName tc
  | otherwise
      = const
      $ stem
      $ fmap toLower
      $ occNameString
      $ getOccName tc


stem :: String -> String
stem "char" = "c"
stem "function" = "func"
stem "bool" = "b"
stem "either" = "e"
stem "error" = "err"
stem "text" = "t"
stem s =
  let syllables = hyphenate english_US s
      (as, bs) = break (not . isLowerVowel . last) syllables
   in join as <>
     case bs of
       [] -> ""
       [b] -> b
       (b : next : _) -> b <>
         takeWhile (not . isLowerVowel) next


isLowerVowel :: Char -> Bool
isLowerVowel 'a' = True
isLowerVowel 'e' = True
isLowerVowel 'i' = True
isLowerVowel 'o' = True
isLowerVowel 'u' = True
isLowerVowel _ = False


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

------------------------------------------------------------------------------
-- | Maybe replace an element in the list if the predicate matches
filterReplace :: (a -> Bool) -> a -> [a] -> [a]
filterReplace f r = fmap (\a -> bool a r $ f a)


------------------------------------------------------------------------------
-- | Produce a unique, good name for a type.
mkGoodName
    :: Set OccName  -- ^ Bindings in scope; used to ensure we don't shadow anything
    -> Type       -- ^ The type to produce a name for
    -> OccName
mkGoodName in_scope (mkTyName -> tn)
  = mkVarOcc
  . fromMaybe (mkNumericSuffix in_scope $ head tn)
  . getFirst
  . foldMap (\n -> bool (pure n) mempty $ check n)
  $ tn <> fmap (<> "'") tn
  where
    check n = S.member (mkVarOcc n) in_scope


mkNumericSuffix :: Set OccName -> String -> String
mkNumericSuffix s nm =
  mappend nm . show . length . filter (isPrefixOf nm . occNameString) $ S.toList s


------------------------------------------------------------------------------
-- | Like 'mkGoodName' but creates several apart names.
mkManyGoodNames
  :: (Traversable t)
  => Set OccName
  -> t Type
  -> t OccName
mkManyGoodNames in_scope args =
  flip evalState in_scope $ for args $ \at -> do
    in_scope <- get
    let n = mkGoodName in_scope at
    modify $ S.insert n
    pure n


------------------------------------------------------------------------------
-- | Which names are in scope?
getInScope :: Map OccName a -> [OccName]
getInScope = M.keys

