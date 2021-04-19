module Wingman.Naming where

import           Control.Arrow
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
import           GhcPlugins (charTy, maybeTyCon)
import           Name
import           TcType
import           Text.Hyphenation (hyphenate, english_US)
import           TyCon
import           Type
import           TysWiredIn (listTyCon, unitTyCon)
import           Wingman.GHC (tcTyVar_maybe)


data Purpose
  = Function [Type] Type
  | Predicate
  | Continuation
  | Integral
  | Number
  | String
  | List Type
  | Maybe Type
  | TyConned TyCon [Type]
  | TyVarred TyVar [Type]

pattern IsPredicate :: Type
pattern IsPredicate <- (tcSplitFunTys -> ([isFunTy -> False], isBoolTy -> True))

pattern IsFunction :: [Type] -> Type -> Type
pattern IsFunction args res <- (tcSplitFunTys -> (args@(_:_), res))

pattern IsString :: Type
pattern IsString <- (splitTyConApp_maybe -> Just ((== listTyCon) -> True, [eqType charTy -> True]))

pattern IsMaybe :: Type -> Type
pattern IsMaybe a <- (splitTyConApp_maybe -> Just ((== maybeTyCon) -> True, [a]))

pattern IsList :: Type -> Type
pattern IsList a <- (splitTyConApp_maybe -> Just ((== listTyCon) -> True, [a]))

pattern IsTyConned :: TyCon -> [Type] -> Type
pattern IsTyConned tc args <- (splitTyConApp_maybe -> Just (id &&& isSymOcc . getOccName -> (tc, False), args))

pattern IsTyVarred :: TyVar -> [Type] -> Type
pattern IsTyVarred v args <- (tcSplitAppTys -> (tcTyVar_maybe -> Just v, args))


getPurposes :: Type -> [Purpose]
getPurposes ty = mconcat
  [ [ Predicate         | IsPredicate         <- [ty] ]
  , [ Function args res | IsFunction args res <- [ty] ]
  , with (isIntegerTy ty) [ Integral, Number          ]
  , with (isIntTy ty)     [ Integral, Number          ]
  , [ Number            | isFloatingTy ty             ]
  , [ String            | isStringTy ty               ]
  , [ Maybe a           | IsMaybe a           <- [ty] ]
  , [ List a            | IsList a            <- [ty] ]
  , [ TyVarred v args   | IsTyVarred v args   <- [ty] ]
  , [ TyConned tc args  | IsTyConned tc args  <- [ty]
                        , not (isTupleTyCon tc)
                        , tc /= listTyCon             ]
  ]


with :: Monoid a => Bool -> a -> a
with False _ = mempty
with True a = a


functionNames :: [String]
functionNames = ["f", "g", "h"]


mkName :: Purpose -> [String]
mkName (Function args res)
  | Just tv_args <- traverse tcTyVar_maybe $ args <> pure res
  = fmap (<> foldMap (occNameString . occName) tv_args) functionNames
mkName (Function _ _) = functionNames
mkName Predicate = pure "p"
mkName Continuation = pure "k"
mkName Integral = ["n", "i", "j"]
mkName Number = ["x", "y", "z", "w"]
mkName String = ["s", "str"]
mkName (List t) = fmap (<> "s") $ mkName =<< getPurposes t
mkName (Maybe t) = fmap ("m_" <>) $ mkName =<< getPurposes t
mkName (TyVarred tv args)
  | Just tv_args <- traverse tcTyVar_maybe args
  = pure $ foldMap (occNameString . occName) $ tv : tv_args
mkName (TyVarred tv _) = pure $ occNameString $ occName tv
mkName (TyConned tc args@(_:_))
  | Just tv_args <- traverse tcTyVar_maybe args
  = pure $ mconcat
      [ mkTyConName tc
      , bool mempty "_" $ length (mkTyConName tc) > 1
      , foldMap (occNameString . occName) tv_args
      ]
mkName (TyConned tc _)
  = pure
  $ mkTyConName tc


mkTyName :: Type -> [String]
mkTyName = mkName <=< getPurposes


------------------------------------------------------------------------------
-- | Get a good name for a type constructor.
mkTyConName :: TyCon -> String
mkTyConName tc
  | tc == unitTyCon = "u"
  | isSymOcc occ
      = take 1
      . fmap toLower
      . filterReplace isSymbol      's'
      . filterReplace isPunctuation 'p'
      $ name
  | camels@(_:_:_) <- camelTerms name
      = foldMap (fmap toLower . take 1) camels
  | otherwise
      = getStem
      $ fmap toLower
      $ name
  where
    occ = getOccName tc
    name = occNameString occ


camelTerms :: String -> [String]
camelTerms = split (== '@') . go2 . go1
  where
    go1 "" = ""
    go1 (x:u:l:xs) | isUpper u && isLower l = x : '@' : u : l : go1 xs
    go1 (x:xs) = x : go1 xs
    go2 "" = ""
    go2 (l:u:xs) | isLower l && isUpper u = l : '@' : u : go2 xs
    go2 (x:xs) = x : go2 xs


getStem :: String -> String
getStem str =
  let s = stem str
   in case (s == str, length str) of
        (False, _)             -> s
        (True, (<= 3) -> True) -> str
        _                      -> take 2 str

stem :: String -> String
stem "char" = "c"
stem "function" = "func"
stem "bool" = "b"
stem "either" = "e"
stem "text" = "txt"
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
  . fromMaybe (mkNumericSuffix in_scope $ fromMaybe "x" $ listToMaybe tn)
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

