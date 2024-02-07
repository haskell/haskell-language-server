{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NoPolyKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Example where

import           Development.IDE.Graph
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Rule
import           GHC.Generics
import           Type.Reflection               (typeRep)

data Rule a = Rule
    deriving (Eq, Generic, Hashable, NFData)

instance Typeable a => Show (Rule a) where
    show Rule = show $ typeRep @a

type instance RuleResult (Rule a) = a

ruleUnit :: Rules ()
ruleUnit = addRule $ \(Rule :: Rule ()) _old _mode -> do
    return $ RunResult ChangedRecomputeDiff "" ()

-- | Depends on Rule @()
ruleBool :: Rules ()
ruleBool = addRule $ \Rule _old _mode -> do
    () <- apply1 Rule
    return $ RunResult ChangedRecomputeDiff "" True
