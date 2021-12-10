module FixityDef where

infixl 3 <!>
(<!>) :: Maybe a -> Maybe (Maybe b) -> Maybe String
(<!>) a b = Nothing
