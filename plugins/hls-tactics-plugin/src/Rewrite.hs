{-# LANGUAGE BangPatterns                       #-}
{-# LANGUAGE DeriveFunctor                      #-}
{-# LANGUAGE DerivingStrategies                 #-}
{-# LANGUAGE GADTs                              #-}
{-# LANGUAGE KindSignatures                     #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE StandaloneDeriving                 #-}
{-# LANGUAGE TupleSections                      #-}
{-# LANGUAGE UndecidableInstances               #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-type-defaults         #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Rewrite where

import Control.Monad.State.Strict
import GHC.Exts
import Control.Applicative
import Data.List (find)
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Checkers
import Data.Functor.Identity
import Test.QuickCheck.Classes ()
import GHC.Generics (Generic)
import Data.Tuple (swap)
import Data.Data (Typeable, eqT, (:~:) (Refl))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Debug.RecoverRTTI
import Debug.Trace (traceM)


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

instance CoArbitrary Judgement where
  coarbitrary (hy :- g) = coarbitrary (hy, g)

instance CoArbitrary Type where
  coarbitrary (TVar l_c) = variant @Int 0 . coarbitrary l_c
  coarbitrary (t :-> t2) = variant @Int 1 . coarbitrary (t, t2)
  coarbitrary (TPair t t2) = variant @Int 2 . coarbitrary (t, t2)

instance CoArbitrary Term where
  coarbitrary (Var l_c) = variant @Int 0 . coarbitrary l_c
  coarbitrary (Hole) = variant @Int 1
  coarbitrary (Lam l_c t) = variant @Int 2 . coarbitrary (l_c, t)
  coarbitrary (Pair t t2) = variant @Int 3 . coarbitrary (t, t2)

instance Arbitrary Term where
  arbitrary
    = let terminal = [Var <$> arbitrary, pure Hole]
      in sized $ \ n -> case n <= 1 of
           True -> oneof terminal
           False -> oneof $
             [ Lam <$> arbitrary <*> scale (subtract 1) arbitrary
             , Pair <$> scale (flip div 2) arbitrary <*> scale (flip div 2) arbitrary
             ] <> terminal

instance Show (ProofState ext err s m a) where
  show = anythingToString

instance (Show ext, Show err, Show jdg, Monoid jdg, Show a) =>
    Show (TacticT jdg ext err s m a) where
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

instance Functor m => MonadState s (ProofState ext err s m) where
  state s = Stateful $ fmap (fmap pure . swap) s

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


instance Functor m => MonadState s (TacticT jdg ext err s m) where
  state s = TacticT $ lift $ Stateful $ fmap (fmap pure . swap) s


deriving instance (Show a, Show jdg, Show (m (Rule jdg ext err s m a))) =>
  Show (Rule jdg ext err s m a)

instance Functor m => MonadState s (Rule jdg ext err s m) where
  state s =  StatefulR $ fmap (fmap pure . swap) s


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

class MonadExtract m ext | m -> ext where
  hole :: m ext

instance Applicative m => MonadExtract m Term where
  hole = pure Hole


kill
    :: Monad m
    => s
    -> (a -> (ext -> m r) -> m r)
    -> (ext -> m r)
    -> m r
    -> (s -> err -> m r)
    -> (m r -> m r -> m r)
    -> ProofState ext err s m a
    -> m r
kill s bind ok cut raise keep (Subgoal a k) = do
  bind a $ kill s bind ok cut raise keep . k

kill s bind ok cut raise keep (Effect m) =
  m >>= kill s bind ok cut raise keep

kill s bind ok cut raise keep (Stateful m) =
  let (s', t) = m s
   in kill s' bind ok cut raise keep t

kill s bind ok cut raise keep (Alt t1 t2) =
  keep (kill s bind ok cut raise keep t1)
       (kill s bind ok cut raise keep t2)

kill s bind ok cut raise keep (Interleave t1 t2) =
  -- TODO(sandy): for now
  keep (kill s bind ok cut raise keep t1)
       (kill s bind ok cut raise keep t2)

kill s bind ok cut raise keep (Commit (t1 :: ProofState ext err s m x) t2 k) = do
  let kill_as_proofstate t =
        kill s
          (\ext k' -> pure $ Subgoal ext $ fmap Effect k')
          (pure . Axiom)
          (pure Empty)
          (\s err -> pure $ put s >> Throw err)
          (liftA2 (<|>))
          t
  (x1 :: ProofState ext err s m x) <-
    kill_as_proofstate t1
  let run_t2 bind' ok' cut' raise keep' = do
        (x2 :: ProofState ext err s m x) <-
          kill_as_proofstate t2
        kill s bind' ok' cut' raise keep' $ k =<< x2

  -- TODO(sandy): ok to ignore k' here?
  kill s (\x k' -> kill s bind ok cut raise keep $ k x) ok
    (run_t2 bind ok cut raise keep)
    (\s' err -> run_t2 bind ok (raise s' err) raise keep) keep $ x1

kill _ _ _ cut _ _ Empty = cut

kill s bind ok cut raise keep (Handle t h k)
  = let bind' = \x k' -> kill s bind ok cut raise keep $ k x
     in kill s bind' ok cut
          (\s' err -> kill s' bind' ok cut raise keep $ h err) keep t

kill s _ _ _ raise _ (Throw err) = raise s err

kill _ _ ok _ _ _ (Axiom ext) = ok ext



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

proof :: Monad m => s -> ProofState Term err s m jdg -> m [Result jdg err Term]
proof s =
  kill s
    (undefined)
    (pure . pure . Extract)
    (pure [])
    (const $ pure . pure . ErrorResult)
    (liftA2 (<>))

proof2 :: (Monad m, MonadExtract m ext) => s -> ProofState Term err s m Judgement -> m [Either err Term]
proof2 s =
  kill s
    (const $ \f -> f =<< hole)
    (pure . pure . Right)
    (do
        -- !_ <- traceM "hit the empty ctor"
        pure [])
    (const $ \err -> do
      -- !_ <- traceM "hit the error ctor"
      pure . pure $ Left err)
    (liftA2 (<>))


runTactic :: Monad m => s -> jdg -> TacticT jdg Term err s m a -> m [Result jdg err Term]
runTactic s jdg (TacticT m) = proof s $ execStateT m jdg

runTactic2 :: (Monad m) => s -> Judgement -> TacticT Judgement Term err s m a -> m [Either err Term]
runTactic2 s jdg (TacticT m) = proof2 s $ execStateT m jdg



-- Just a very simple version of Simply Typed Lambda Calculus,
-- augmented with 'Hole' so that we can have
-- incomplete extracts.
data Term
  = Var String
  | Hole
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
testJdg = [("a1", "a"), ("bee", "b"), ("c", "c")] :- TPair "a" (TPair "b" "c")

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
         , Typeable a
         , Functor m
         , Arbitrary jdg
         , Arbitrary (m (Rule jdg ext err s m ext))
         ) => Arbitrary (TacticT jdg ext err s m a) where
  arbitrary = oneof
    [ arb
    , case eqT @a @() of
        Just Refl -> fmap rule arbitrary
        Nothing -> arb
    ]
    where
      arb = TacticT . lift <$> arbitrary

instance  ( Arbitrary a
          , Arbitrary jdg
          , CoArbitrary s
          , Arbitrary s
          , Arbitrary (m (Rule jdg ext err s m a))
          , CoArbitrary ext
          ) => Arbitrary (Rule jdg ext err s m a) where
  arbitrary
    = let terminal = [Pure <$> arbitrary]
      in sized $ \n -> case n <= 1 of
           True  -> oneof terminal
           False -> oneof $
            [ SubgoalR <$> arbitrary <*> scale (subtract 1) arbitrary
            , EffectR <$> scale (subtract 1) arbitrary
            , StatefulR <$> scale (subtract 1) arbitrary
            ] <> terminal

instance ( Arbitrary s
         , Monad m
         , EqProp (m [Result a err Term])
         ) => EqProp (ProofState Term err s m a) where
  a =-= b = property $ do
    s <- arbitrary @s
    pure $ proof @m s a =-= proof s b

instance ( Arbitrary s
         , Arbitrary jdg
         , Monad m
         , EqProp (m [Result jdg err Term])
         ) => EqProp (Rule jdg Term err s m Term) where
  a =-= b = rule a =-= rule b

instance ( Arbitrary s
         , Monad m
         , Arbitrary jdg
         , EqProp (m [Result jdg err Term])
         ) => EqProp (TacticT jdg Term err s m a) where
  a =-= b = property $ do
    s <- arbitrary @s
    jdg <- arbitrary @jdg
    pure $ runTactic @m s jdg a =-= runTactic s jdg b

instance {-# OVERLAPPING #-}
         ( Arbitrary s
         , Monad m
         , EqProp (m [Either err Term])
         ) => EqProp (TacticT Judgement Term err s m a) where
  a =-= b = property $ do
    s <- arbitrary @s
    jdg <- arbitrary @Judgement
    pure $ runTactic2 @m s jdg a =-= runTactic2 s jdg b

instance Arbitrary Type where
  arbitrary
    = let terminal = [TVar <$> arbitrary]
      in sized $ \ n ->
        case n <= 1 of
          True  -> oneof terminal
          False -> oneof $
            [ (:->) <$> scale (flip div 2) arbitrary
                    <*> scale (flip div 2) arbitrary
            , TPair <$> scale (flip div 2) arbitrary
                    <*> scale (flip div 2) arbitrary
            ] <> terminal

instance Arbitrary Judgement where
  arbitrary = (:-) <$> scale (flip div 3) arbitrary <*> scale (flip div 2) arbitrary

type ProofStateTest = ProofState Term String Int Identity
type TacticTest = TacticT Judgement Term String Int Identity
type RuleTest = Rule Judgement Term String Int Identity

spec :: Spec
spec = do
  prop "commit x empty is x" $ \(t :: TT) ->
    commit t empty =-= t

  prop "left distrib of <|> over >>=" $ \(t1 :: TI) t2 (t3 :: Int -> TT) ->
    ((t1 <|> t2) >>= t3)
      =-= ((t1 >>= t3) <|> (t2 >>= t3))

  prop "put distrib over alt" $ \(t1 :: TT) t2 s ->
    (put s >> (t1 <|> t2))
      =-= ((put s >> t1) <|> (put s >> t2))

  prop "alt rolls back state" $ \(t :: TT) s ->
    ((put s >> empty) <|> t)
      =-= t

  prop "catch of throw is just the handler" $ \err f ->
    (catch (throw err) f :: TT)
      =-= f err

  prop "catch with rethrowing is id" $ \(t :: TT) ->
    catch t throw
      =-= t

  prop "state is persistent across throw" $ \s e ->
    catch (put s >> throw e) (const $ get >>= mkResult)
      =-= mkResult s

  prop "state is persistent across rule" $ \s ->
    (put s >> (rule $ get >>= pure . Var . show))
      =-= mkResult s

  prop "commit rolls back state" $ \(t :: TT) s ->
    ((put s >> empty) `commit` t)
      =-= t

  prop "alt takes handling preference over throw" $ \e f ->
    (catch (throw e <|> pure ()) f)
      =-= (pure () :: TT)

  prop "commit a rule always succeeds" $ \r t ->
    ((commit (rule r) t) :: TT)
      =-= rule r

  prop "commit semantics" $ \(t :: TT) (m :: TT) err ->
    ((commit (pure ()) t >> m >> throw err) :: TT)
      =-= (m >> throw err)

  prop "commit of pure" $ \(i :: Int) (t :: TI) ->
    (commit (pure i) t)
      =-= pure i

  prop "commit runs its continuation" $ \(i :: Int) (t :: TI) f ->
    ((commit (pure i) t >> f) :: TT)
      =-= f



main :: IO ()
main = hspec spec
    -- quickBatch $ functor     (undefined :: ProofStateTest (Int, Int, Int))
    -- quickBatch $ applicative (undefined :: ProofStateTest (Int, Int, Int))
    -- quickBatch $ alternative (undefined :: ProofStateTest Int)
    -- quickBatch $ monad       (undefined :: ProofStateTest (Int, Int, Int))
    -- quickBatch $ monadPlus   (undefined :: ProofStateTest (Int, Int))
    -- quickBatch $ monadState  (undefined :: ProofStateTest (Int, Int))

    -- quickBatch $ functor     (undefined :: RuleTest (Term, Term, Term))
    -- quickBatch $ applicative (undefined :: RuleTest (Term, Term, Term))
    -- quickBatch $ monad       (undefined :: RuleTest (Term, Term, Term))
    -- -- quickBatch $ monadState  (undefined :: RuleTest (Term, Term))

--     quickBatch $ functor     (undefined :: TacticTest ((), (), ()))
--     quickBatch $ applicative (undefined :: TacticTest ((), (), ()))
--     quickBatch $ alternative (undefined :: TacticTest ())
--     quickBatch $ monad       (undefined :: TacticTest ((), (), ()))
--     quickBatch $ monadPlus   (undefined :: TacticTest ((), ()))
--     quickBatch $ monadState  (undefined :: TacticTest ((), ()))
    -- -- fails, m1 could be lift, then you do an action twice in the second case
    -- quickCheck $ property $ \(t1 :: TI) t2 (t3 :: TT) ->
    --   (t1 >> (t2 <|> t3))
    --     =-= ((t1 >> t2) <|> (t1 >> t3))
    -- -- should fail! this is the broken commit test
    -- quickCheck $ property $ \(t1 :: TI) t2 (t3 :: Int -> TT) ->
    --   ((commit t1 t2) >>= t3)
    --     =-= ((t1 >>= t3) `commit` (t2 >>= t3))


mkResult :: Show a => a -> TT
mkResult = rule . pure . Var . show

catch
    :: Functor m
    => TacticT jdg ext err s m a
    -> (err -> TacticT jdg ext err s m a)
    -> TacticT jdg ext err s m a
catch (TacticT t) h = TacticT $ StateT $ \jdg ->
  Handle
    (runStateT t jdg)
    (flip runStateT jdg . unTacticT . h)
    pure


type TI = TacticTest Int
type TT = TacticTest ()

monadState
    :: forall m s a b
     . ( MonadState s m
       , EqProp (m s)
       , EqProp (m ())
       , Show s
       , Arbitrary s
       )
    => m (a, b)
    -> TestBatch
monadState _ =
  ( "MonadState laws"
  , [ ("get >> get", (get >> get) =-= get @s @m)
    , ("get >>= put", (get @s @m >>= put) =-= pure ())
    , ("put >> put", property $ do
        s1 <- arbitrary
        s2 <- arbitrary
        pure $
          counterexample (show s1) $
          counterexample (show s2) $
            (put @_ @m s1 >> put s2) =-= put s2
      )
    , ("put >> get", property $ do
        s <- arbitrary
        pure $
          counterexample (show s) $
            (put s >> get) =-= (put s >> pure @m s)
      )
    ]
  )


test :: [Either String Term]
test =
  runIdentity $ runTactic2 (0 :: Int) testJdg $ do
    commit pair (throw "no")

