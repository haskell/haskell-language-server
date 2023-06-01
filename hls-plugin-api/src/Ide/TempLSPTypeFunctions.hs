{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
module Ide.TempLSPTypeFunctions (takeLefts, dumpNulls, nullToMaybe', NullToMaybe,
                                 toLspId, toTypedResponseError) where
import           Data.Aeson                    (FromJSON (parseJSON), ToJSON,
                                                decode, encode, fromJSON)
import           Data.Aeson.Types              (parseMaybe)
import           Data.Semigroup                ()
import           Data.Text                     (Text)
import           Language.LSP.Protocol.Message (ErrorData,
                                                LspId (IdInt, IdString),
                                                ResponseError (ResponseError),
                                                TResponseError (TResponseError))
import           Language.LSP.Protocol.Types   (Int32, Null,
                                                WorkspaceEdit (WorkspaceEdit),
                                                type (|?) (..))


-- The functions below may be added to the lsp-types package if they end up being
-- useful. temporarily including them here now.


takeLefts :: Foldable f => f (a |? b) -> [a]
takeLefts = foldr (\x acc -> case x of
                                InL x' -> x' : acc
                                InR _  -> acc) []

-- Especially when we want to use concat, we are not interested in nulls,
-- because of this we need to filter them out
dumpNulls :: (Foldable f, NullToMaybe a b) => f a -> [b]
dumpNulls = foldr (\x acc -> case nullToMaybe' x of
                                Just x' -> x' : acc
                                Nothing -> acc) []

instance Semigroup s => Semigroup (s |? Null) where
  InL x <> InL y = InL (x <> y)
  InL x <> InR _ = InL x
  InR _ <> InL x = InL x
  InR _ <> InR y = InR y

instance Semigroup WorkspaceEdit where
  (WorkspaceEdit a b c) <> (WorkspaceEdit a' b' c') = WorkspaceEdit (a <> a') (b <> b') (c <> c')

class NullToMaybe a b where
  nullToMaybe' :: a -> Maybe b

instance NullToMaybe (a |? Null) a where
  nullToMaybe' (InL x) = Just x
  nullToMaybe' (InR _) = Nothing

instance NullToMaybe (a |? (b |? Null)) (a |? b) where
  nullToMaybe' (InL x)       = Just $ InL x
  nullToMaybe' (InR (InL x)) = Just $ InR x
  nullToMaybe' (InR (InR _)) = Nothing

instance NullToMaybe (a |? (b |? (c |? Null))) (a |? (b |? c)) where
  nullToMaybe' (InL x)             = Just $ InL x
  nullToMaybe' (InR (InL x))       = Just $ InR $ InL x
  nullToMaybe' (InR (InR (InL x))) = Just $ InR $ InR x
  nullToMaybe' (InR (InR (InR _))) = Nothing

toLspId :: (Int32 |? Text) -> LspId a
toLspId (InL x) = IdInt x
toLspId (InR y) = IdString y

toTypedResponseError :: FromJSON (ErrorData m) => ResponseError -> TResponseError m
toTypedResponseError (ResponseError c m d) = TResponseError c m (decode . encode=<<  d)
