{-# LANGUAGE RankNTypes #-}

------------------------------------------------------------------------------
-- | Things that belong in the future release of refinery v5.
module Refinery.Future
  ( runStreamingTacticT
  , hoistListT
  , consume
  ) where

import Control.Applicative
import Control.Monad (ap, (>=>))
import Control.Monad.State.Lazy (runStateT)
import Control.Monad.Trans
import Data.Either (isRight)
import Data.Functor ((<&>))
import Data.Tuple (swap)
import Refinery.ProofState
import Refinery.Tactic.Internal



hoistElem :: Functor m => (forall x. m x -> n x) -> Elem m a -> Elem n a
hoistElem _ Done = Done
hoistElem f (Next a lt) = Next a $ hoistListT f lt


hoistListT :: Functor m => (forall x. m x -> n x) -> ListT m a -> ListT n a
hoistListT f t = ListT $ f $ fmap (hoistElem f) $ unListT t


consume :: Monad m => ListT m a -> (a -> m ()) -> m ()
consume lt f = unListT lt >>= \case
  Done -> pure ()
  Next a lt' -> f a >> consume lt' f


newHole :: MonadExtract meta ext err s m => s -> m (s, (meta, ext))
newHole = fmap swap . runStateT hole

runStreamingTacticT :: (MonadExtract meta ext err s m) => TacticT jdg ext err s m () -> jdg -> s -> ListT m (Either err (Proof s meta jdg ext))
runStreamingTacticT t j s = streamProofs s $ fmap snd $ proofState t j

data Elem m a
    = Done
    | Next a (ListT m a)
    deriving stock Functor


point :: Applicative m => a -> Elem m a
point a = Next a $ ListT $ pure Done

newtype ListT m a = ListT { unListT :: m (Elem m a) }

cons :: (Applicative m) => a -> ListT m a -> ListT m a
cons x xs = ListT $ pure $ Next x xs

instance Functor m => Functor (ListT m) where
    fmap f (ListT xs) = ListT $ xs <&> \case
        Done -> Done
        Next a xs -> Next (f a) (fmap f xs)

instance (Monad m) => Applicative (ListT m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Alternative (ListT m) where
    empty = ListT $ pure Done
    (ListT xs) <|> (ListT ys) =
        ListT $ xs >>= \case
          Done -> ys
          Next x xs -> pure (Next x (xs <|> ListT ys))

instance (Monad m) => Monad (ListT m) where
    return a = cons a empty
    (ListT xs) >>= k =
        ListT $ xs >>= \case
          Done -> pure Done
          Next x xs -> unListT $ k x <|> (xs >>= k)


instance MonadTrans ListT where
    lift m = ListT $ fmap (\x -> Next x empty) m


interleaveT :: (Monad m) => Elem m a -> Elem m a -> Elem m a
interleaveT xs ys =
    case xs of
      Done -> ys
      Next x xs -> Next x $ ListT $ fmap (interleaveT ys) $ unListT xs

--         ys <&> \case
--           Done -> Next x xs
--           Next y ys -> Next x (cons y (interleaveT xs ys))

force :: (Monad m) => Elem m a -> m [a]
force = \case
    Done -> pure []
    Next x xs' -> (x:) <$> (unListT xs' >>= force)

ofList :: Monad m => [a] -> Elem m a
ofList [] = Done
ofList (x:xs) = Next x $ ListT $ pure $ ofList xs

streamProofs :: forall ext err s m goal meta. (MonadExtract meta ext err s m) => s -> ProofStateT ext ext err s m goal -> ListT m (Either err (Proof s meta goal ext))
streamProofs s p = ListT $ go s [] pure p
    where
      go :: s -> [(meta, goal)] -> (err -> m err) -> ProofStateT ext ext err s m goal -> m (Elem m (Either err (Proof s meta goal ext)))
      go s goals _ (Subgoal goal k) = do
         (s', (meta, h)) <- newHole s
         -- Note [Handler Reset]:
         -- We reset the handler stack to avoid the handlers leaking across subgoals.
         -- This would happen when we had a handler that wasn't followed by an error call.
         --     pair >> goal >>= \g -> (handler_ $ \_ -> traceM $ "Handling " <> show g) <|> failure "Error"
         -- We would see the "Handling a" message when solving for b.
         go s' (goals ++ [(meta, goal)]) pure $ k h
      go s goals handlers (Effect m) = m >>= go s goals handlers
      go s goals handlers (Stateful f) =
          let (s', p) = f s
          in go s' goals handlers p
      go s goals handlers (Alt p1 p2) =
          unListT $ ListT (go s goals handlers p1) <|> ListT (go s goals handlers p2)
      go s goals handlers (Interleave p1 p2) =
          interleaveT <$> go s goals handlers p1 <*> go s goals handlers p2
      go s goals handlers (Commit p1 p2) = do
          solns <- force =<< go s goals handlers p1
          if any isRight solns then pure $ ofList solns else go s goals handlers p2
      go _ _ _ Empty = pure Done
      go _ _ handlers (Failure err _) = do
          annErr <- handlers err
          pure $ point $ Left annErr
      go s goals handlers (Handle p h) =
          -- Note [Handler ordering]:
          -- If we have multiple handlers in scope, then we want the handlers closer to the error site to
          -- run /first/. This allows the handlers up the stack to add their annotations on top of the
          -- ones lower down, which is the behavior that we desire.
          -- IE: for @handler f >> handler g >> failure err@, @g@ ought to be run before @f@.
          go s goals (h >=> handlers) p
      go s goals _ (Axiom ext) = pure $ point $ Right (Proof ext s goals)

