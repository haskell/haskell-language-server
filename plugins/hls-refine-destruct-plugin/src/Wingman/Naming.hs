{-# LANGUAGE CPP #-}

module Wingman.Naming where

import           Control.Arrow
import           Control.Monad.State.Strict
import           Data.Aeson (camelTo2)
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
import           Development.IDE.GHC.Compat.Core hiding (IsFunction)
import           Text.Hyphenation (hyphenate, english_US)
import           Wingman.GHC (tcTyVar_maybe)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Utils.TcType
#endif


------------------------------------------------------------------------------
-- | A classification of a variable, for which we have specific naming rules.
-- A variable can have multiple purposes simultaneously.
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
    -- ^ Something of the form @TC a b c@
  | TyVarred TyVar [Type]
    -- ^ Something of the form @m a b c@

pattern IsPredicate :: Type
pattern IsPredicate <-
  (tcSplitFunTys -> ([isFunTy . scaledThing -> False], isBoolTy -> True))

pattern IsFunction :: [Type] -> Type -> Type
pattern IsFunction args res <-
  (first (map scaledThing) . tcSplitFunTys -> (args@(_:_), res))

pattern IsString :: Type
pattern IsString <-
  (splitTyConApp_maybe -> Just ((== listTyCon) -> True, [eqType charTy -> True]))

pattern IsMaybe :: Type -> Type
pattern IsMaybe a <-
  (splitTyConApp_maybe -> Just ((== maybeTyCon) -> True, [a]))

pattern IsList :: Type -> Type
pattern IsList a <-
  (splitTyConApp_maybe -> Just ((== listTyCon) -> True, [a]))

pattern IsTyConned :: TyCon -> [Type] -> Type
pattern IsTyConned tc args <-
  (splitTyConApp_maybe -> Just (id &&& isSymOcc . getOccName -> (tc, False), args))

pattern IsTyVarred :: TyVar -> [Type] -> Type
pattern IsTyVarred v args <-
  (tcSplitAppTys -> (tcTyVar_maybe -> Just v, args))


------------------------------------------------------------------------------
-- | Get the 'Purpose's of a type. A type can have multiple purposes
-- simultaneously, so the order of purposes in this function corresponds to the
-- precedence of that naming rule. Which means, eg, that if a type is both
-- a 'Predicate' and a 'Function', we should prefer to use the predicate naming
-- rules, since they come first.
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


------------------------------------------------------------------------------
-- | Return 'mempty' if the give bool is false.
with :: Monoid a => Bool -> a -> a
with False _ = mempty
with True a = a


------------------------------------------------------------------------------
-- | Names we can give functions
functionNames :: [String]
functionNames = ["f", "g", "h"]


------------------------------------------------------------------------------
-- | Get a ranked ordering of names for a given purpose.
purposeToName :: Purpose -> [String]
purposeToName (Function args res)
  | Just tv_args <- traverse tcTyVar_maybe $ args <> pure res
  = fmap (<> foldMap (occNameString . occName) tv_args) functionNames
purposeToName (Function _ _) = functionNames
purposeToName Predicate = pure "p"
purposeToName Continuation = pure "k"
purposeToName Integral = ["n", "i", "j"]
purposeToName Number = ["x", "y", "z", "w"]
purposeToName String = ["s", "str"]
purposeToName (List t) = fmap (<> "s") $ purposeToName =<< getPurposes t
purposeToName (Maybe t) = fmap ("m_" <>) $ purposeToName =<< getPurposes t
purposeToName (TyVarred tv args)
  | Just tv_args <- traverse tcTyVar_maybe args
  = pure $ foldMap (occNameString . occName) $ tv : tv_args
purposeToName (TyVarred tv _) = pure $ occNameString $ occName tv
purposeToName (TyConned tc args@(_:_))
  | Just tv_args <- traverse tcTyVar_maybe args
  = [ mkTyConName tc
      -- We insert primes to everything later, but it gets the lowest
      -- precedence. Here we'd like to prefer it over the more specific type
      -- name.
    , mkTyConName tc <> "'"
    , mconcat
      [ mkTyConName tc
      , bool mempty "_" $ length (mkTyConName tc) > 1
      , foldMap (occNameString . occName) tv_args
      ]
    ]
purposeToName (TyConned tc _)
  = pure
  $ mkTyConName tc


mkTyName :: Type -> [String]
mkTyName = purposeToName <=< getPurposes


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
      $ fmap toLower name
  where
    occ = getOccName tc
    name = occNameString occ


------------------------------------------------------------------------------
-- | Split a string into its camel case components.
camelTerms :: String -> [String]
camelTerms = split (== '@') . camelTo2 '@'


------------------------------------------------------------------------------
-- | A stem of a string is either a special-case shortened form, or a shortened
-- first syllable. If the string is one syllable, we take the full word if it's
-- short, or just the first two characters if it's long. Otherwise, just take
-- the first syllable.
--
-- NOTE: There's no rhyme or reason here, I just experimented until I got
-- results that were reasonably consistent with the names I would give things.
getStem :: String -> String
getStem str =
  let s = stem str
   in case (s == str, length str) of
        (False, _)             -> s
        (True, (<= 3) -> True) -> str
        _                      -> take 2 str

------------------------------------------------------------------------------
-- | Get a special-case stem, or, failing that, give back the first syllable.
stem :: String -> String
stem "char" = "c"
stem "function" = "func"
stem "bool" = "b"
stem "either" = "e"
stem "text" = "txt"
stem s = join $ take 1 $ hyphenate english_US s


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
    check n = S.member (mkVarOcc n) $ illegalNames <> in_scope


illegalNames :: Set OccName
illegalNames = S.fromList $ fmap mkVarOcc
  [ "case"
  , "of"
  , "class"
  , "data"
  , "do"
  , "type"
  , "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "mdo"
  , "newtype"
  , "proc"
  , "rec"
  , "where"
  ]



------------------------------------------------------------------------------
-- | Given a desired name, compute a new name for it based on how many names in
-- scope conflict with it. Eg, if we want to name something @x@, but already
-- have @x@, @x'@ and @x2@ in scope, we will give back @x3@.
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

