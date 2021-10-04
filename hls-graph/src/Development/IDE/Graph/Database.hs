
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
module Development.IDE.Graph.Database(
    ShakeDatabase,
    ShakeValue,
    shakeOpenDatabase,
    shakeRunDatabase,
    shakeRunDatabaseForKeys,
    shakeProfileDatabase,
    ) where

import           Data.Dynamic
import           Data.Maybe
import           Development.IDE.Graph.Classes ()
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Database
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Profile  (writeProfile)
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
shakeRunDatabase = shakeRunDatabaseForKeys Nothing

-- Only valid if we never pull on the results, which we don't
unvoid :: Functor m => m () -> m a
unvoid = fmap undefined

shakeRunDatabaseForKeys
    :: Maybe [Key]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> ShakeDatabase
    -> [Action a]
    -> IO ([a], [IO ()])
shakeRunDatabaseForKeys keysChanged (ShakeDatabase lenAs1 as1 db) as2 = do
    incDatabase db keysChanged
    as <- fmap (drop lenAs1) $ runActions db $ map unvoid as1 ++ as2
    return (as, [])

-- | Given a 'ShakeDatabase', write an HTML profile to the given file about the latest run.
shakeProfileDatabase :: ShakeDatabase -> FilePath -> IO ()
shakeProfileDatabase (ShakeDatabase _ _ s) file = writeProfile file s
