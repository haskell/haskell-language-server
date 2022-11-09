{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Custom SYB traversals
module Wingman.Judgements.SYB where

import           Data.Foldable (foldl')
import           Data.Generics hiding (typeRep)
import qualified Data.Text as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util (unpackFS)
import           GHC.Exts (Any)
import           Type.Reflection
import           Unsafe.Coerce (unsafeCoerce)
import           Wingman.StaticPlugin (pattern WingmanMetaprogram)


------------------------------------------------------------------------------
-- | Like 'everything', but only looks inside 'Located' terms that contain the
-- given 'SrcSpan'.
everythingContaining
    :: forall r
     . Monoid r
    => SrcSpan
    -> GenericQ r
    -> GenericQ r
everythingContaining dst f = go
  where
    go :: GenericQ r
    go x =
      case genericIsSubspan dst x of
        Just False -> mempty
        _ -> foldl' (<>) (f x) (gmapQ go x)


------------------------------------------------------------------------------
-- | Helper function for implementing 'everythingWithin'
--
-- NOTE(sandy): Subtly broken. In an ideal world, this function should return
-- @Just False@ for nodes of /any type/ which do not contain the span. But if
-- this functionality exists anywhere within the SYB machinery, I have yet to
-- find it.
genericIsSubspan
    :: SrcSpan
    -> GenericQ (Maybe Bool)
genericIsSubspan dst = mkQ1 (L noSrcSpan ()) Nothing $ \case
  L span _ -> Just $ dst `isSubspanOf` span


------------------------------------------------------------------------------
-- | Like 'mkQ', but allows for polymorphic instantiation of its specific case.
-- This instantiation matches whenever the dynamic value has the same
-- constructor as the proxy @f ()@ value.
mkQ1 :: forall a r f
      . (Data a, Data (f ()))
     => f ()                  -- ^ Polymorphic constructor to match on
     -> r                     -- ^ Default value
     -> (forall b. f b -> r)  -- ^ Polymorphic match
     -> a
     -> r
mkQ1 proxy r br a =
    case l_con == a_con && sameTypeModuloLastApp @a @(f ()) of
      -- We have proven that the two values share the same constructor, and
      -- that they have the same type if you ignore the final application.
      -- Therefore, it is safe to coerce @a@ to @f b@, since @br@ is universal
      -- over @b@ and can't inspect it.
      True  -> br $ unsafeCoerce @_ @(f Any) a
      False -> r
  where
    l_con = toConstr proxy
    a_con = toConstr a


------------------------------------------------------------------------------
-- | Given @a ~ f1 a1@ and @b ~ f2 b2@, returns true if @f1 ~ f2@.
sameTypeModuloLastApp :: forall a b. (Typeable a, Typeable b) => Bool
sameTypeModuloLastApp =
  let tyrep1 = typeRep @a
      tyrep2 = typeRep @b
   in case (tyrep1 , tyrep2) of
        (App a _, App b _) ->
          case eqTypeRep a b of
            Just HRefl -> True
            Nothing    -> False
        _ -> False


metaprogramAtQ :: SrcSpan -> GenericQ [(SrcSpan, T.Text)]
metaprogramAtQ ss = everythingContaining ss $ mkQ mempty $ \case
  L new_span (WingmanMetaprogram program) -> pure (new_span, T.pack $ unpackFS program)
  (_ :: LHsExpr GhcTc) -> mempty


metaprogramQ :: GenericQ [(SrcSpan, T.Text)]
metaprogramQ = everything (<>) $ mkQ mempty $ \case
  L new_span (WingmanMetaprogram program) -> pure (new_span, T.pack $ unpackFS program)
  (_ :: LHsExpr GhcTc) -> mempty

