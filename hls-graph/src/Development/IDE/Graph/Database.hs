
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
module Development.IDE.Graph.Database(
    ShakeDatabase,
    ShakeValue,
    shakeOpenDatabase,
    shakeRunDatabase,
    shakeRunDatabaseForKeys,
    shakeProfileDatabase,
    shakeGetBuildStep,
    shakeGetDirtySet,
    shakeGetCleanKeys
    ,shakeGetBuildEdges) where
import           Data.Dynamic
import           Data.IORef                              (readIORef)
import           Data.Maybe
import           Development.IDE.Graph.Classes           ()
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Database
import qualified Development.IDE.Graph.Internal.Ids      as Ids
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

-- | Returns the set of dirty keys annotated with their age (in # of builds)
shakeGetDirtySet :: ShakeDatabase -> IO [(Key, Int)]
shakeGetDirtySet (ShakeDatabase _ _ db) =
    fmap snd <$> Development.IDE.Graph.Internal.Database.getDirtySet db

-- | Returns the build number
shakeGetBuildStep :: ShakeDatabase -> IO Int
shakeGetBuildStep (ShakeDatabase _ _ db) = do
    Step s <- readIORef $ databaseStep db
    return s

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

-- | Returns the clean keys in the database
shakeGetCleanKeys :: ShakeDatabase -> IO [(Key, Result )]
shakeGetCleanKeys (ShakeDatabase _ _ db) = do
    keys <- Ids.elems $ databaseValues db
    return [ (k,res) | (k, Clean res) <- keys]

-- | Returns the total count of edges in the build graph
shakeGetBuildEdges :: ShakeDatabase -> IO Int
shakeGetBuildEdges (ShakeDatabase _ _ db) = do
    keys <- Ids.elems $ databaseValues db
    let ress = mapMaybe (getResult . snd) keys
    return $ sum $ map (length . getResultDepsDefault [] . resultDeps) ress
