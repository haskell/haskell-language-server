{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Example where

import Development.IDE.Graph
import Development.IDE.Graph.Rule
import Development.IDE.Graph.Classes
import GHC.Generics
import Type.Reflection (typeRep)

data Rule a = Rule
    deriving (Eq, Generic, Hashable, NFData)

instance Typeable a => Show (Rule a) where
    show Rule = show $ typeRep @a

type instance RuleResult (Rule a) = a

ruleUnit :: Rules ()
ruleUnit = addRule $ \(Rule :: Rule ()) old mode -> do
    return $ RunResult ChangedRecomputeDiff "" ()

-- | Depends on Rule @()
ruleBool :: Rules ()
ruleBool = addRule $ \Rule old mode -> do
    () <- apply1 Rule
    return $ RunResult ChangedRecomputeDiff "" True
