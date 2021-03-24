{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Rewrite where

import Control.Monad.State.Strict
import GHC.Exts
import Control.Applicative
import Data.List (find)
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Checkers
import Data.Functor.Identity
import Test.QuickCheck.Classes
import GHC.Generics (Generic)


data ProofState ext err s m a where
  Subgoal
    :: a
    -> (ext -> ProofState ext err s m a)
    -> ProofState ext err s m a
  Effect
    :: m (ProofState ext err s m a)
    -> ProofState ext err s m a
  Stateful
    :: (s -> (s, ProofState ext err s m a))
    -> ProofState ext err s m a
  Alt
    :: ProofState ext err s m a
    -> ProofState ext err s m a
    -> ProofState ext err s m a
  Interleave
    :: ProofState ext err s m a
    -> ProofState ext err s m a
    -> ProofState ext err s m a
  Commit
    :: ProofState ext err s m x
    -> ProofState ext err s m x
    -> (x -> ProofState ext err s m a)
    -> ProofState ext err s m a
  Empty
    :: ProofState ext err s m a
  Handle
    :: ProofState ext err s m x
    -> (err -> ProofState ext err s m x)
    -> (x -> ProofState ext err s m a)
    -> ProofState ext err s m a
  Throw
    :: err
    -> ProofState ext err s m a
  Axiom
    :: ext
    -> ProofState ext err s m a

instance CoArbitrary Term where
  coarbitrary (Var l_c) = variant @Int 0 . coarbitrary l_c
  coarbitrary (Hole i) = variant @Int 1 . coarbitrary i
  coarbitrary (Lam l_c t) = variant @Int 2 . coarbitrary (l_c, t)
  coarbitrary (Pair t t2) = variant @Int 3 . coarbitrary (t, t2)

instance Arbitrary Term where
  arbitrary
    = let terminal = [Var <$> arbitrary, Hole <$> arbitrary]
      in
        sized
          $ (\ n
               -> case n <= 1 of
                    True -> oneof terminal
                    False
                      -> oneof
                           $ ([(Lam <$> arbitrary) <*> scale (subtract 1) arbitrary,
                               (Pair <$> scale (flip div 2) arbitrary)
                                 <*> scale (flip div 2) arbitrary]
                                <> terminal))

instance Show (ProofState ext err s m a) where
  show (Subgoal a fextpexterrsma) = "Subgoal a fextpexterrsma"
  show (Effect mpexterrsma) = "Effect mpexterrsma"
  show (Stateful fsp_spexterrsma) = "Stateful fsp_spexterrsma"
  show (Alt pexterrsma pexterrsma2) = "Alt pexterrsma pexterrsma2"
  show (Interleave pexterrsma pexterrsma2) = "Interleave pexterrsma pexterrsma2"
  show (Commit pexterrsmx pexterrsmx2 fxpexterrsma) = "Commit pexterrsmx pexterrsmx2 fxpexterrsma"
  show Empty = "Empty"
  show (Handle pexterrsmx ferrpexterrsmx fxpexterrsma) = "Handle pexterrsmx ferrpexterrsmx fxpexterrsma"
  show (Throw err) = "Throw err"
  show (Axiom ext) = "Axiom ext"

instance Monoid jdg => Show (TacticT jdg ext err s m a) where
  show (TacticT t) = show $ runStateT t mempty


instance Functor m => Functor (ProofState ext err s m) where
  fmap f (Subgoal a k)
    = Subgoal (f a) $ fmap f . k
  fmap f (Effect m)
    = Effect $ fmap (fmap f) m
  fmap f (Stateful m)
    = Stateful $ fmap (fmap (fmap f)) m
  fmap f (Alt t1 t2)
    = Alt (fmap f t1) (fmap f t2)
  fmap f (Interleave t1 t2)
    = Interleave (fmap f t1) (fmap f t2)
  fmap f (Commit t1 t2 k)
    = Commit t1 t2 $ fmap f . k
  fmap _ Empty
    = Empty
  fmap _ (Throw err)
    = Throw err
  fmap f (Handle t1 h k)
    = Handle t1 h $ fmap f . k
  fmap _ (Axiom ext)
    = Axiom ext

instance Functor m => Applicative (ProofState ext err s m) where
  pure a = Subgoal a Axiom
  (<*>) = ap

instance Functor m => Monad (ProofState ext err s m) where
  return = pure
  (>>=) (Subgoal a k) f = applyCont (f <=< k) $ f a
  (>>=) (Effect m) f = Effect $ fmap (f =<<) m
  (>>=) (Stateful m) f = Stateful $ fmap (fmap (f =<<)) m
  (>>=) (Alt t1 t2) f = Alt (t1 >>= f) (t2 >>= f)
  (>>=) (Interleave t1 t2) f = Interleave (f =<< t1) (f =<< t2)
  (>>=) (Commit t1 t2 k) f = Commit t1 t2 $ f <=< k
  (>>=) Empty _ = Empty
  (>>=) (Handle t h k) f = Handle t h $ f <=< k
  (>>=) (Throw err) _ = Throw err
  (>>=) (Axiom ext) _ = Axiom ext

instance Functor m => Alternative (ProofState ext err s m) where
  empty = Empty
  (<|>) = Alt

instance Functor m => MonadPlus (ProofState ext err s m) where
  mzero = empty
  mplus = (<|>)


applyCont
    :: (Functor m)
    => (ext -> ProofState ext err s m a)
    -> ProofState ext err s m a
    -> ProofState ext err s m a
applyCont k (Subgoal goal k')
  = Subgoal goal (applyCont k . k')
applyCont k (Effect m)
  = Effect (fmap (applyCont k) m)
applyCont k (Stateful s)
  = Stateful $ fmap (applyCont k) . s
applyCont k (Alt p1 p2)
  = Alt (applyCont k p1) (applyCont k p2)
applyCont k (Interleave p1 p2)
  = Interleave (applyCont k p1) (applyCont k p2)
applyCont k (Commit p1 p2 k')
  = Commit p1 p2 $ applyCont k . k'
applyCont k (Handle t h k')
  = Handle t h $ applyCont k . k'
applyCont _ Empty
  = Empty
applyCont _ (Throw err)
  = Throw err
applyCont k (Axiom ext)
  = k ext


newtype TacticT jdg ext err s m a = TacticT
  { unTacticT :: StateT jdg (ProofState ext err s m) a
  } deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus)


data Rule jdg ext err s m a where
  Pure
    :: a
    -> Rule jdg ext err s m a
  SubgoalR
    :: jdg
    -> (ext -> Rule jdg ext err s m a)
    -> Rule jdg ext err s m a
  EffectR
    :: m (Rule jdg ext err s m a)
    -> Rule jdg ext err s m a
  StatefulR
    :: (s -> (s, Rule jdg ext err s m a))
    -> Rule jdg ext err s m a
  deriving stock (Functor)


instance Functor m => Applicative (Rule jdg ext err s m) where
  pure a = Pure a
  (<*>) = ap


instance Functor m => Monad (Rule jdg ext err s m) where
  return = pure
  (>>=) (Pure a) f = f a
  (>>=) (SubgoalR jdg k) f = SubgoalR jdg $ f <=< k
  (>>=) (EffectR m) f = EffectR $ fmap (f =<<) m
  (>>=) (StatefulR m) f = StatefulR $ fmap (fmap (f =<<)) m


rule
    :: Functor m
    => Rule jdg ext err s m ext
    -> TacticT jdg ext err s m ()
rule r = TacticT $ StateT $ const $ fmap ((), ) $ ruleToProof r


ruleToProof
    :: Functor m
    => Rule jdg ext err s m ext
    -> ProofState ext err s m jdg
ruleToProof (Pure ext)
  = Axiom ext
ruleToProof (SubgoalR jdg k)
  = Subgoal jdg $ ruleToProof . k
ruleToProof (EffectR m)
  = Effect $ fmap ruleToProof m
ruleToProof (StatefulR m)
  = Stateful $ fmap (fmap ruleToProof) m


goal :: Functor m => TacticT jdg ext err s m jdg
goal = TacticT get


subgoal :: Functor m => jdg -> Rule jdg ext err s m ext
subgoal jdg = SubgoalR jdg pure


throw :: Functor m => err -> TacticT jdg ext err s m a
throw = TacticT . lift . Throw


pair :: Functor m => TacticT Judgement Term String s m ()
pair = do
  goal >>= \case
    hy :- TPair ta tb -> rule $ do
      exta <- subgoal $ hy :- ta
      extb <- subgoal $ hy :- tb
      pure $ Pair exta extb
    _ -> throw "not a pair"


assumption :: Functor m => TacticT Judgement Term String s m ()
assumption = do
  hy :- g <- goal
  case find ((== g) . snd) hy of
    Just v -> rule $ pure $ Var $ fst v
    Nothing -> throw $ "nothing in scope for " <> show g


kill
    :: Monad m
    => s
    -> (a -> m r)
    -> (ext -> m r)
    -> m r
    -> (err -> m r)
    -> ProofState ext err s m a
    -> m r
kill _ bind _ _ _ (Subgoal a k) =
  bind a
kill s bind success failure throw (Effect m) =
  m >>= kill s bind success failure throw
kill s bind success failure throw (Stateful m) =
  let (s', t) = m s
   in kill s' bind success failure throw t
kill s bind success failure throw (Alt t1 t2) =
  kill s bind success (kill s bind success failure throw t2)
    (\err -> kill s bind success (throw err) throw t2) t1
kill s bind success failure throw (Interleave t1 t2) =
  kill s bind success (kill s bind success failure throw t2)
    (\err -> kill s bind success (throw err) throw t2) t1
kill s bind success failure throw (Commit t1 t2 k)
  = kill s
      (kill s bind success failure throw . k)
      success
              (kill s (kill s bind success failure throw . k) success failure     throw t2)
      (\err -> kill s (kill s bind success failure throw . k) success (throw err) throw t2)
      t1
kill _ _ _ failure _ Empty = failure
kill s bind success failure throw (Handle t h k)
  = kill s
      (kill s bind success failure throw . k)
      success
      failure
      (\err -> kill s (kill s bind success failure throw . k) success failure throw $ h err) t
kill _ _ _ _ throw (Throw err) = throw err
kill _ _ success _ _ (Axiom ext) = success ext


data Result jdg err ext
  = HoleResult jdg
  | ErrorResult err
  | Extract ext
  | NoResult
  deriving stock (Show, Generic)

instance (EqProp jdg, EqProp err, EqProp ext) => EqProp (Result jdg err ext)
instance EqProp Term
instance EqProp Judgement
instance EqProp Type

proof :: Monad m => s -> ProofState ext err s m jdg -> m (Result jdg err ext)
proof s = kill s (pure . HoleResult) (pure . Extract) (pure NoResult) (pure . ErrorResult)


runTactic :: Monad m => s -> jdg -> TacticT jdg ext err s m () -> m (Result jdg err ext)
runTactic s jdg (TacticT m) = proof s $ execStateT m jdg



-- Just a very simple version of Simply Typed Lambda Calculus,
-- augmented with 'Hole' so that we can have
-- incomplete extracts.
data Term
  = Var String
  | Hole Int
  | Lam String Term
  | Pair Term Term
  deriving stock (Show, Eq, Generic)


-- The type part of simply typed lambda calculus
data Type
  = TVar String
  | Type :-> Type
  | TPair Type Type
  deriving stock (Show, Eq, Generic)

infixr 4 :->

instance IsString Type where
    fromString = TVar


-- A judgement is just a context, along with a goal
data Judgement = [(String, Type)] :- Type
  deriving stock (Show, Eq, Generic)


commit :: Functor m => TacticT jdg ext err s m a -> TacticT jdg ext err s m a -> TacticT jdg ext err s m a
commit (TacticT t1) (TacticT t2) = TacticT $ StateT $ \jdg ->
  Commit (runStateT t1 jdg) (runStateT t2 jdg) pure

auto :: Functor m => TacticT Judgement Term String s m ()
auto = do
  commit pair assumption
  auto


testJdg :: Judgement
testJdg = [("a1", "a"), ("bee", "b")] :- TPair "a" (TPair "b" "c")

instance Semigroup Judgement where
  (<>) = error "no semigroup"

instance Monoid Judgement where
  mempty = [] :- TVar ""


instance ( Arbitrary err
         , CoArbitrary err
         , Arbitrary ext
         , CoArbitrary ext
         , Arbitrary a
         , CoArbitrary s
         , Arbitrary s
         , Arbitrary (m (ProofState ext err s m a))
         , Arbitrary (m (ProofState ext err s m Int))
         ) => Arbitrary (ProofState ext err s m a) where
  arbitrary =
    let terminal = [pure Empty, Throw <$> arbitrary, Axiom <$> arbitrary]
     in sized $ \n -> case n <= 1 of
          True  -> oneof terminal
          False -> oneof $
            [ Subgoal <$> arbitrary <*> scale (subtract 1) arbitrary
            , Effect <$> scale (subtract 1) arbitrary
            , Stateful <$> scale (subtract 1) arbitrary
            , Alt <$> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
            , Interleave <$> scale (flip div 2) arbitrary
                         <*> scale (flip div 2) arbitrary
            , Commit <$> scale (flip div 3) (arbitrary @(ProofState ext err s m Int))
                     <*> scale (flip div 3) arbitrary
                     <*> scale (flip div 3) arbitrary
            , Handle <$> scale (flip div 3) (arbitrary @(ProofState ext err s m Int))
                     <*> scale (flip div 3) arbitrary
                     <*> scale (flip div 3) arbitrary
            ] <> terminal

instance ( Arbitrary err
         , CoArbitrary err
         , Arbitrary ext
         , CoArbitrary ext
         , Arbitrary a
         , CoArbitrary s
         , Arbitrary s
         , Arbitrary (m (ProofState ext err s m a))
         , Arbitrary (m (ProofState ext err s m Int))
         , Functor m
         ) => Arbitrary (TacticT jdg ext err s m a) where
  arbitrary = TacticT . lift <$> arbitrary

instance ( Arbitrary s
         , Monad m
         , EqProp (m (Result a err ext))
         ) => EqProp (ProofState ext err s m a) where
  a =-= b = property $ do
    s <- arbitrary @s
    pure $ proof @m s a =-= proof s b

instance ( Arbitrary s
         , Monad m
         , Arbitrary jdg
         , EqProp (m (Result jdg err ext))
         ) => EqProp (TacticT jdg ext err s m ()) where
  a =-= b = property $ do
    s <- arbitrary @s
    jdg <- arbitrary @jdg
    pure $ runTactic @m s jdg a =-= runTactic s jdg b

instance Arbitrary Type where
  arbitrary
    = let terminal = [TVar <$> arbitrary]
      in
        sized
          $ (\ n
               -> case n <= 1 of
                    True -> oneof terminal
                    False
                      -> oneof
                           $ ([((:->) <$> scale (flip div 2) arbitrary)
                                 <*> scale (flip div 2) arbitrary,
                               (TPair <$> scale (flip div 2) arbitrary)
                                 <*> scale (flip div 2) arbitrary]
                                <> terminal))

instance Arbitrary Judgement where
  arbitrary = (:-) <$> arbitrary <*> arbitrary

type ProofStateTest = ProofState Term String Int Identity
type TacticTest = TacticT Judgement Term String Int Identity

main :: IO ()
main = do
    -- quickBatch $ functor     (undefined :: ProofStateTest (Int, Int, Int))
    -- quickBatch $ applicative (undefined :: ProofStateTest (Int, Int, Int))
    -- quickBatch $ alternative (undefined :: ProofStateTest Int)
    -- quickBatch $ monad       (undefined :: ProofStateTest (Int, Int, Int))
    -- quickBatch $ monadPlus   (undefined :: ProofStateTest (Int, Int))

    -- quickBatch $ functor     (undefined :: TacticTest ((), (), ()))
    -- quickBatch $ applicative (undefined :: TacticTest ((), (), ()))
    -- quickBatch $ alternative (undefined :: TacticTest ())
    -- quickBatch $ monad       (undefined :: TacticTest ((), (), ()))
    -- quickBatch $ monadPlus   (undefined :: TacticTest ((), ()))

    quickCheck $ property $ \(t :: TacticTest ()) (m :: TacticTest ()) err ->
      ((commit (pure ()) t >> m >> throw err) :: TacticTest ())
        =-= (m >> throw err)


