{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes  #-}

-- | Custom SYB traversals explicitly designed for operating over the GHC AST.
module Generics.SYB.GHC
    ( genericIsSubspan,
      mkBindListT,
      everywhereM',
      smallestM,
      largestM
    ) where

import           Control.Monad
import           Data.Functor.Compose          (Compose (Compose))
import           Data.Monoid                   (Any (Any))
import           Development.IDE.GHC.Compat
import           Development.IDE.Graph.Classes
import           Generics.SYB


-- | A generic query intended to be used for calling 'smallestM' and
-- 'largestM'. If the current node is a 'Located', returns whether or not the
-- given 'SrcSpan' is a subspan. For all other nodes, returns 'Nothing', which
-- indicates uncertainty. The search strategy in 'smallestM' et al. will
-- continue searching uncertain nodes.
genericIsSubspan ::
    forall ast.
    Typeable ast =>
    -- | The type of nodes we'd like to consider.
    Proxy (Located ast) ->
    SrcSpan ->
    GenericQ (Maybe (Bool, ast))
genericIsSubspan _ dst = mkQ Nothing $ \case
  (L span ast :: Located ast) -> Just (dst `isSubspanOf` span, ast)


-- | Lift a function that replaces a value with several values into a generic
-- function. The result doesn't perform any searching, so should be driven via
-- 'everywhereM' or friends.
--
-- The 'Int' argument is the index in the list being bound.
mkBindListT :: forall b m. (Data b, Monad m) => (Int -> b -> m [b]) -> GenericM m
mkBindListT f = mkM $ fmap join . traverse (uncurry f) . zip [0..]


-- | Apply a monadic transformation everywhere in a top-down manner.
everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
    where
        go :: GenericM m
        go = gmapM go <=< f


------------------------------------------------------------------------------
-- Custom SYB machinery
------------------------------------------------------------------------------

-- | Generic monadic transformations that return side-channel data.
type GenericMQ r m = forall a. Data a => a -> m (r, a)

------------------------------------------------------------------------------
-- | Apply the given 'GenericM' at all every node whose children fail the
-- 'GenericQ', but which passes the query itself.
--
-- The query must be a monotonic function when it returns 'Just'. That is, if
-- @s@ is a subtree of @t@, @q t@ should return @Just True@ if @q s@ does. It
-- is the True-to-false edge of the query that triggers the transformation.
--
-- Why is the query a @Maybe Bool@? The GHC AST intersperses 'Located' nodes
-- with data nodes, so for any given node we can only definitely return an
-- answer if it's a 'Located'. See 'genericIsSubspan' for how this parameter is
-- used.
smallestM :: forall m a. Monad m => GenericQ (Maybe (Bool, a)) -> (a -> GenericM m) -> GenericM m
smallestM q f = fmap snd . go
  where
    go :: GenericMQ Any m
    go x = do
      case q x of
        Nothing -> gmapMQ go x
        Just (True, a) -> do
          it@(r, x') <- gmapMQ go x
          case r of
            Any True  -> pure it
            Any False -> fmap (Any True,) $ f a x'
        Just (False, _) -> pure (mempty, x)

------------------------------------------------------------------------------
-- | Apply the given 'GenericM' at every node that passes the 'GenericQ', but
-- don't descend into children if the query matches. Because this traversal is
-- root-first, this policy will find the largest subtrees for which the query
-- holds true.
--
-- Why is the query a @Maybe Bool@? The GHC AST intersperses 'Located' nodes
-- with data nodes, so for any given node we can only definitely return an
-- answer if it's a 'Located'. See 'genericIsSubspan' for how this parameter is
-- used.
largestM :: forall m a. Monad m => GenericQ (Maybe (Bool, a)) -> (a -> GenericM m) -> GenericM m
largestM q f = go
  where
    go :: GenericM m
    go x = do
      case q x of
        Just (True, a)  -> f a x
        Just (False, _) -> pure x
        Nothing    -> gmapM go x

newtype MonadicQuery r m a = MonadicQuery
  { runMonadicQuery :: m (r, a)
  }
  deriving stock (Functor)
  deriving Applicative via Compose m ((,) r)


------------------------------------------------------------------------------
-- | Like 'gmapM', but also returns side-channel data.
gmapMQ ::
    forall f r a. (Monoid r, Data a, Applicative f) =>
    (forall d. Data d => d -> f (r, d)) ->
    a ->
    f (r, a)
gmapMQ f = runMonadicQuery . gfoldl k pure
  where
    k :: Data d => MonadicQuery r f (d -> b) -> d -> MonadicQuery r f b
    k c x = c <*> MonadicQuery (f x)
