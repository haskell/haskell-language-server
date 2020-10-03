{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Tactic.Naming where

import           Control.Monad.State.Strict
import           Data.Bool (bool)
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Traversable
import           Name
import           TcType
import           TyCon
import           Type
import           TysWiredIn (listTyCon, pairTyCon, unitTyCon)


------------------------------------------------------------------------------
-- | Use type information to create a reasonable name.
mkTyName :: Type -> String
-- eg. mkTyName (a -> B) = "fab"
mkTyName (tcSplitFunTys -> ([a@(isFunTy -> False)], b))
  = "f" ++ mkTyName a ++ mkTyName b
-- eg. mkTyName (a -> b -> C) = "f_C"
mkTyName (tcSplitFunTys -> ((_:_), b))
  = "f_" ++ mkTyName b
-- eg. mkTyName (Either A B) = "eab"
mkTyName (splitTyConApp_maybe -> Just (c, args))
  = mkTyConName c ++ foldMap mkTyName args
-- eg. mkTyName a = "a"
mkTyName (getTyVar_maybe -> Just tv)
  = occNameString $ occName tv
-- eg. mkTyName (forall x. y) = "y"
mkTyName (tcSplitSigmaTy -> ((_:_), _, t))
  = mkTyName t
mkTyName _ = "x"


------------------------------------------------------------------------------
-- | Get a good name for a type constructor.
mkTyConName :: TyCon -> String
mkTyConName tc
  | tc == listTyCon = "l_"
  | tc == pairTyCon = "p_"
  | tc == unitTyCon = "unit"
  | otherwise
      = take 1
      . fmap toLower
      . filterReplace isSymbol      's'
      . filterReplace isPunctuation 'p'
      . occNameString
      $ getOccName tc


------------------------------------------------------------------------------
-- | Maybe replace an element in the list if the predicate matches
filterReplace :: (a -> Bool) -> a -> [a] -> [a]
filterReplace f r = fmap (\a -> bool a r $ f a)


------------------------------------------------------------------------------
-- | Produce a unique, good name for a type.
mkGoodName
    :: [OccName]  -- ^ Bindings in scope; used to ensure we don't shadow anything
    -> Type       -- ^ The type to produce a name for
    -> OccName
mkGoodName in_scope t =
  let tn = mkTyName t
   in mkVarOcc $ case elem (mkVarOcc tn) in_scope of
        True -> tn ++ show (length in_scope)
        False -> tn


------------------------------------------------------------------------------
-- | Like 'mkGoodName' but creates several apart names.
mkManyGoodNames
  :: (Traversable t, Monad m)
  => M.Map OccName a
  -> t Type
  -> m (t OccName)
mkManyGoodNames hy args =
  flip evalStateT (getInScope hy) $ for args $ \at -> do
    in_scope <- get
    let n = mkGoodName in_scope at
    modify (n :)
    pure n


------------------------------------------------------------------------------
-- | Which names are in scope?
getInScope :: Map OccName a -> [OccName]
getInScope = M.keys

