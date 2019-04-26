-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}

-- | A Shake implementation of the compiler service.
--
--   There are two primary locations where data lives, and both of
--   these contain much the same data:
--
-- * The Shake database (inside 'shakeDb') stores a map of shake keys
--   to shake values. In our case, these are all of type 'Q' to 'A'.
--   During a single run all the values in the Shake database are consistent
--   so are used in conjunction with each other, e.g. in 'uses'.
--
-- * The 'Values' type stores a map of keys to values. These values are
--   always stored as real Haskell values, whereas Shake serialises all 'A' values
--   between runs. To deserialise a Shake value, we just consult Values.
--   Additionally, Values can be used in an inconsistent way, for example
--   useStale.
module Development.IDE.State.Shake(
    IdeState,
    IdeRule, IdeResult,
    shakeOpen, shakeShut,
    shakeRun,
    shakeProfile,
    useStale,
    use, uses,
    use_, uses_,
    define, defineEarlyCutoff,
    getAllDiagnostics, unsafeClearAllDiagnostics,
    reportSeriousError, reportSeriousErrorDie,
    IsIdeGlobal, addIdeGlobal, getIdeGlobalState, getIdeGlobalAction,
    garbageCollect,
    setPriority,
    sendEvent,
    shakeLogDebug,
    shakeLogInfo,
    shakeLogWarning,
    shakeLogError,
    ) where

import           Development.Shake
import           Development.Shake.Database
import           Development.Shake.Classes
import           Development.Shake.Rule
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as BS
import           Data.Dynamic
import           Data.Maybe
import           Data.Either
import           Data.List.Extra
import qualified Data.Text as T
import Development.IDE.Logger as Logger
import Development.IDE.Types.LSP
import           Development.IDE.Types.Diagnostics
import           Control.Concurrent.Extra
import           Control.Exception
import           Control.DeepSeq
import           System.Time.Extra
import           Data.Typeable
import           Data.Tuple.Extra
import           System.FilePath
import qualified Development.Shake as Shake
import           Control.Monad.Extra
import qualified Data.Set as Set
import           Data.Time
import           System.IO.Unsafe
import           Numeric.Extra



-- information we stash inside the shakeExtra field
data ShakeExtras = ShakeExtras
    {eventer :: Event -> IO ()
    ,logger :: Logger.Handle IO
    ,globals :: Var (Map.HashMap TypeRep Dynamic)
    ,state :: Var Values
    }

getShakeExtras :: Action ShakeExtras
getShakeExtras = do
    Just x <- getShakeExtra @ShakeExtras
    return x

getShakeExtrasRules :: Rules ShakeExtras
getShakeExtrasRules = do
    Just x <- getShakeExtraRules @ShakeExtras
    return x



class Typeable a => IsIdeGlobal a where

addIdeGlobal :: IsIdeGlobal a => a -> Rules ()
addIdeGlobal x@(typeOf -> ty) = do
    ShakeExtras{globals} <- getShakeExtrasRules
    liftIO $ modifyVar_ globals $ \mp -> case Map.lookup ty mp of
        Just _ -> error $ "Can't addIdeGlobal twice on the same type, got " ++ show ty
        Nothing -> return $! Map.insert ty (toDyn x) mp


getIdeGlobalExtras :: forall a . IsIdeGlobal a => ShakeExtras -> IO a
getIdeGlobalExtras ShakeExtras{globals} = do
    Just x <- Map.lookup (typeRep (Proxy :: Proxy a)) <$> readVar globals
    return $ fromDyn x $ error "Serious error, corrupt globals"

getIdeGlobalAction :: forall a . IsIdeGlobal a => Action a
getIdeGlobalAction = liftIO . getIdeGlobalExtras =<< getShakeExtras

getIdeGlobalState :: forall a . IsIdeGlobal a => IdeState -> IO a
getIdeGlobalState = getIdeGlobalExtras . shakeExtras


-- | The state of the all values - nested so you can easily find all errors at a given file.
type Values =
    Map.HashMap FilePath
        (Map.HashMap Key
            (IdeResult Dynamic)
        )


-- | Key type
data Key = forall k . (Typeable k, Hashable k, Eq k, Show k) => Key k

instance Show Key where
  show (Key k) = show k

instance Eq Key where
    Key k1 == Key k2 | Just k2' <- cast k2 = k1 == k2'
                     | otherwise = False

instance Hashable Key where
    hashWithSalt salt (Key key) = hashWithSalt salt key

-- | The result of an IDE operation. Warnings and errors are in the Diagnostic,
--   and a value is in the Maybe. For operations that throw an error you
--   expect a non-empty list of diagnostics, at least one of which is an error,
--   and a Nothing. For operations that succeed you expect perhaps some warnings
--   and a Just. For operations that depend on other failing operations you may
--   get empty diagnostics and a Nothing, to indicate this phase throws no fresh
--   errors but still failed.
--
--   A rule on a file should only return diagnostics for that given file. It should
--   not propagate diagnostic errors through multiple phases.
type IdeResult v = ([Diagnostic], Maybe v)

type IdeRule k v =
  ( Shake.RuleResult k ~ v
  , Shake.ShakeValue k
  , Show v
  , Typeable v
  , NFData v
  )


-- | A Shake database plus persistent store. Can be thought of as storing
--   mappings from @(FilePath, k)@ to @RuleResult k@.
data IdeState = IdeState
    {shakeDb :: ShakeDatabase
    ,shakeAbort :: Var (IO ()) -- close whoever was running last
    ,shakeClose :: IO ()
    ,shakeExtras :: ShakeExtras
    }


profileDir :: Maybe FilePath
profileDir = Nothing -- set to Just the directory you want profile reports to appear in


-- This is debugging code that generates a series of profiles, if the Boolean is true
shakeRunDatabaseProfile :: ShakeDatabase -> [Action a] -> IO [a]
shakeRunDatabaseProfile shakeDb acts = do
        (time, (res,_)) <- duration $ shakeRunDatabase shakeDb acts
        whenJust profileDir $ \dir -> do
            count <- modifyVar profileCounter $ \x -> let y = x+1 in return (y,y)
            let file = "ide-" ++ profileStartTime ++ "-" ++ takeEnd 5 ("0000" ++ show count) ++ "-" ++ showDP 2 time <.> "html"
            shakeProfileDatabase shakeDb $ dir </> file
        return res
    where

{-# NOINLINE profileStartTime #-}
profileStartTime :: String
profileStartTime = unsafePerformIO $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime

{-# NOINLINE profileCounter #-}
profileCounter :: Var Int
profileCounter = unsafePerformIO $ newVar 0

setValues :: IdeRule k v
          => Var Values
          -> k
          -> FilePath
          -> IdeResult v
          -> IO (Maybe [Diagnostic], [Diagnostic]) -- ^ (before, after)
setValues state key file val = modifyVar state $ \inVal -> do
    let k = Key key
        outVal = Map.insertWith Map.union file (Map.singleton k $ second (fmap toDyn) val) inVal
        f = concatMap fst . Map.elems
    return (outVal, (f <$> Map.lookup file inVal, f $ outVal Map.! file))

-- | The outer Maybe is Nothing if this function hasn't been computed before
--   the inner Maybe is Nothing if the result of the previous computation failed to produce
--   a value
getValues :: forall k v. IdeRule k v => Var Values -> k -> FilePath -> IO (Maybe (Maybe v))
getValues state key file = do
    vs <- readVar state
    return $ do
        f <- Map.lookup file vs
        v <- Map.lookup (Key key) f
        pure $ fmap (fromJust . fromDynamic @v) $ snd v

-- | Open a 'IdeState', should be shut using 'shakeShut'.
shakeOpen :: (Event -> IO ()) -- ^ diagnostic handler
          -> Logger.Handle IO
          -> ShakeOptions
          -> Rules ()
          -> IO IdeState
shakeOpen diags shakeLogger opts rules = do
    shakeExtras <- ShakeExtras diags shakeLogger <$> newVar Map.empty <*> newVar Map.empty
    (shakeDb, shakeClose) <- shakeOpenDatabase opts{shakeExtra = addShakeExtra shakeExtras $ shakeExtra opts} rules
    shakeAbort <- newVar $ return ()
    shakeDb <- shakeDb
    return IdeState{..}

shakeProfile :: IdeState -> FilePath -> IO ()
shakeProfile IdeState{..} = shakeProfileDatabase shakeDb

shakeShut :: IdeState -> IO ()
shakeShut = shakeClose

-- | Spawn immediately, add an action to collect the results syncronously.
--   If you are already inside a call to shakeRun that will be aborted with an exception.
shakeRun :: IdeState -> [Action a] -> IO (IO [a])
-- FIXME: If there is already a shakeRun queued up and waiting to send me a kill, I should probably
--        not even start, which would make issues with async exceptions less problematic.
shakeRun IdeState{shakeExtras=ShakeExtras{..}, ..} acts = modifyVar shakeAbort $ \stop -> do
    (stopTime,_) <- duration stop
    Logger.logInfo logger $ T.pack $ "Starting shakeRun (aborting the previous one took " ++ showDuration stopTime ++ ")"
    bar <- newBarrier
    start <- offsetTime
    thread <- forkFinally (shakeRunDatabaseProfile shakeDb acts) $ \res -> do
        signalBarrier bar res
        runTime <- start
        Logger.logInfo logger $ T.pack $
            "Finishing shakeRun (took " ++ showDuration runTime ++ ", " ++ (if isLeft res then "exception" else "completed") ++ ")"
    -- important: we send an async exception to the thread, then wait for it to die, before continuing
    return (do killThread thread; void $ waitBarrier bar, either throwIO return =<< waitBarrier bar)

-- | Use the last stale value, if it's ever been computed.
useStale
    :: IdeRule k v
    => IdeState -> k -> FilePath -> IO (Maybe v)
useStale IdeState{shakeExtras=ShakeExtras{state}} k fp =
    join <$> getValues state k fp


getAllDiagnostics :: IdeState -> IO [Diagnostic]
getAllDiagnostics IdeState{shakeExtras = ShakeExtras{state}} = do
    val <- readVar state
    return $ concatMap (concatMap fst . Map.elems) $ Map.elems val

-- | FIXME: This function is temporary! Only required because the files of interest doesn't work
unsafeClearAllDiagnostics :: IdeState -> IO ()
unsafeClearAllDiagnostics IdeState{shakeExtras = ShakeExtras{state}} = modifyVar_ state $
    return . Map.map (Map.map (\(_, x) -> ([], x)))

-- | Clear the results for all files that do not match the given predicate.
garbageCollect :: (FilePath -> Bool) -> Action ()
garbageCollect keep = do
    ShakeExtras{state} <- getShakeExtras
    liftIO $ modifyVar_ state $ return . Map.filterWithKey (\file _ -> keep file)

define
    :: IdeRule k v
    => (k -> FilePath -> Action (IdeResult v)) -> Rules ()
define op = defineEarlyCutoff $ \k v -> (Nothing,) <$> op k v

use :: IdeRule k v
    => k -> FilePath -> Action (Maybe v)
use key file = head <$> uses key [file]

use_ :: IdeRule k v => k -> FilePath -> Action v
use_ key file = head <$> uses_ key [file]

uses_ :: IdeRule k v => k -> [FilePath] -> Action [v]
uses_ key files = do
    res <- uses key files
    case sequence res of
        Nothing -> liftIO $ throwIO BadDependency
        Just v -> return v

reportSeriousError :: String -> Action ()
reportSeriousError t = do
    ShakeExtras{logger} <- getShakeExtras
    liftIO $ Logger.logError logger $ T.pack t

reportSeriousErrorDie :: String -> Action a
reportSeriousErrorDie t = do
    ShakeExtras{logger} <- getShakeExtras
    liftIO $ Logger.logError logger $ T.pack t
    fail t


-- | When we depend on something that reported an error, and we fail as a direct result, throw BadDependency
--   which short-circuits the rest of the action
data BadDependency = BadDependency deriving Show
instance Exception BadDependency

isBadDependency :: SomeException -> Bool
isBadDependency x
    | Just (x :: ShakeException) <- fromException x = isBadDependency $ shakeExceptionInner x
    | Just (_ :: BadDependency) <- fromException x = True
    | otherwise = False


newtype Q k = Q (k, FilePath)
    deriving (Eq,Hashable,Binary,NFData)

instance Show k => Show (Q k) where
    show (Q (k, file)) = show k ++ "; " ++ file

-- | Invariant: the 'v' must be in normal form (fully evaluated).
--   Otherwise we keep repeatedly 'rnf'ing values taken from the Shake database
data A v = A (Maybe v) (Maybe BS.ByteString)
    deriving Show

instance NFData (A v) where rnf (A v x) = v `seq` rnf x

-- In the Shake database we only store one type of key/result pairs,
-- namely Q (question) / A (answer).
type instance RuleResult (Q k) = A (RuleResult k)


-- | Compute the value
uses :: IdeRule k v
    => k -> [FilePath] -> Action [Maybe v]
uses key files = map (\(A value _) -> value) <$> apply (map (Q . (key,)) files)

defineEarlyCutoff
    :: IdeRule k v
    => (k -> FilePath -> Action (Maybe BS.ByteString, IdeResult v))
    -> Rules ()
defineEarlyCutoff op = addBuiltinRule noLint noIdentity $ \(Q (key, file)) old mode -> do
    ShakeExtras{state} <- getShakeExtras
    val <- case old of
        Just old | mode == RunDependenciesSame -> do
            v <- liftIO $ getValues state key file
            case v of
                Just v -> return $ Just $ RunResult ChangedNothing old $ A v (unwrap old)
                _ -> return Nothing
        _ -> return Nothing
    case val of
        Just res -> return res
        Nothing -> do
            (bs, res) <- actionCatch
                (do v <- op key file; liftIO $ evaluate $ force v) $
                \(e :: SomeException) -> pure (Nothing, ([ideErrorText file $ T.pack $ show e | not $ isBadDependency e],Nothing))
            res <- return $ first (map $ fixDiagnostic file) res

            let badErrors = filter (\d -> null file || dRange d == noRange) $ fst res
            when (badErrors /= []) $
                reportSeriousError $ "Bad errors found for " ++ show (key, file) ++ " got " ++ show badErrors

            (before, after) <- liftIO $ setValues state key file res
            updateFileDiagnostics file before after
            let eq = case (bs, fmap unwrap old) of
                    (Just a, Just (Just b)) -> a == b
                    _ -> False
            return $ RunResult
                (if eq then ChangedRecomputeSame else ChangedRecomputeDiff)
                (wrap bs)
                $ A (snd res) bs
    where
        wrap = maybe BS.empty (BS.cons '_')
        unwrap x = if BS.null x then Nothing else Just $ BS.tail x


-- | If any diagnostic has the wrong filename, generate a new diagnostic with the right file name
fixDiagnostic :: FilePath -> Diagnostic -> Diagnostic
fixDiagnostic x d
    | dFilePath d == x = d
    | otherwise = d{dFilePath = x, dRange = noRange, dMessage = T.pack ("Originally reported at " ++ dFilePath d ++ "\n") <> dMessage d}


updateFileDiagnostics ::
     FilePath
  -> Maybe [Diagnostic] -- ^ previous results for this file
  -> [Diagnostic] -- ^ current results
  -> Action ()
updateFileDiagnostics afp previousAll currentAll = do
    let filt = Set.fromList . filter (\x -> dFilePath x == afp)
        previous = fmap filt previousAll
        current = filt currentAll
    when (Just current /= previous) $
        sendEvent $ EventFileDiagnostics $ FileDiagnostics afp $ Set.toList current


setPriority :: (Enum a) => a -> Action ()
setPriority p =
    deprioritize (fromIntegral . negate $ fromEnum p)

sendEvent :: Event -> Action ()
sendEvent e = do
    ShakeExtras{eventer} <- getShakeExtras
    liftIO $ eventer e

-- | bit of an odd signature because we're trying to remove priority
sl :: (Handle IO -> T.Text -> IO ()) -> IdeState -> T.Text -> IO ()
sl f IdeState{shakeExtras=ShakeExtras{logger}} p = f logger p

shakeLogDebug, shakeLogInfo, shakeLogWarning, shakeLogError
    :: IdeState -> T.Text -> IO ()
shakeLogDebug = sl logDebug
shakeLogInfo = sl logInfo
shakeLogWarning = sl logWarning
shakeLogError = sl logError
