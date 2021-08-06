
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
module Development.IDE.Graph.Database(
    ShakeDatabase,
    ShakeValue,
    SomeShakeValue(..),
    shakeOpenDatabase,
    shakeRunDatabase,
    shakeRunDatabaseForKeys,
    shakeProfileDatabase,
    ) where

import           Data.Dynamic
import           Data.Maybe
import           Data.Typeable                           (cast)
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types

data ShakeDatabase = ShakeDatabase !Int [Action ()] Database

-- Placeholder to be the 'extra' if the user doesn't set it
data NonExportedType = NonExportedType

shakeOpenDatabase :: ShakeOptions -> Rules () -> IO (IO ShakeDatabase, IO ())
shakeOpenDatabase opts rules = pure (shakeNewDatabase opts rules, pure ())

shakeNewDatabase :: ShakeOptions -> Rules () -> IO ShakeDatabase
shakeNewDatabase opts rules = do
    let extra = fromMaybe (toDyn NonExportedType) $ shakeExtra opts
    (theRules, actions) <- runRules extra rules
    db <- newDatabase extra theRules
    pure $ ShakeDatabase (length actions) actions db

shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO ([a], [IO ()])
shakeRunDatabase (ShakeDatabase lenAs1 as1 db) as2 = do
    incDatabase db
    as <- fmap (drop lenAs1) $ runActions db $ map unvoid as1 ++ as2
    return (as, [])

-- Only valid if we never pull on the results, which we don't
unvoid :: Functor m => m () -> m a
unvoid = fmap undefined

-- Noop
shakeProfileDatabase :: ShakeDatabase -> FilePath -> IO ()
shakeProfileDatabase _ file = writeFile file ""

data SomeShakeValue = forall k . ShakeValue k => SomeShakeValue k
instance Eq SomeShakeValue where SomeShakeValue a == SomeShakeValue b = cast a == Just b
instance Hashable SomeShakeValue where hashWithSalt s (SomeShakeValue x) = hashWithSalt s x
instance Show SomeShakeValue where show (SomeShakeValue x) = show x

type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, NFData a, Binary a)

shakeRunDatabaseForKeys
    :: Maybe [SomeShakeValue]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO ([a], [IO ()])
shakeRunDatabaseForKeys _keys a b =
    -- Shake upstream does not accept the set of keys changed yet
    -- https://github.com/ndmitchell/shake/pull/802
    shakeRunDatabase a b
