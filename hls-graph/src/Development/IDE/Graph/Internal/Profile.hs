{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

{- HLINT ignore "Redundant bracket" -} -- a result of CPP expansion

module Development.IDE.Graph.Internal.Profile (writeProfile) where

import           Control.Concurrent.STM.Stats            (readTVarIO)
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8              as LBS
import           Data.Char
import           Data.Dynamic                            (toDyn)
import qualified Data.HashMap.Strict                     as Map
import           Data.List                               (dropWhileEnd,
                                                          intercalate,
                                                          partition, sort,
                                                          sortBy)
import           Data.List.Extra                         (nubOrd)
import           Data.Maybe
import           Data.Time                               (getCurrentTime)
import           Data.Time.Format.ISO8601                (iso8601Show)
import           Development.IDE.Graph.Internal.Database (getDirtySet)
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Paths
import           Development.IDE.Graph.Internal.Types
import qualified Language.Javascript.DGTable             as DGTable
import qualified Language.Javascript.Flot                as Flot
import qualified Language.Javascript.JQuery              as JQuery
import           Numeric.Extra                           (showDP)
import           System.FilePath
import           System.IO.Unsafe                        (unsafePerformIO)
import           System.Time.Extra                       (Seconds)

#if !MIN_VERSION_base(4,20,0)
import           Data.List                               (foldl')
#endif

#ifdef FILE_EMBED
import           Data.FileEmbed
import           Language.Haskell.TH.Syntax              (runIO)
#endif

-- | Generates an report given some build system profiling data.
writeProfile :: FilePath -> Database -> IO ()
writeProfile out db = do
    (report, mapping) <- toReport db
    dirtyKeysMapped <- do
        dirtyIds <- fromListKeySet . fmap fst <$> getDirtySet db
        let dirtyKeysMapped = mapMaybe (`lookupKeyMap` mapping) . toListKeySet $ dirtyIds
        return $ Just $ sort dirtyKeysMapped
    rpt <- generateHTML dirtyKeysMapped report
    LBS.writeFile out rpt

data ProfileEntry = ProfileEntry
    {prfName :: !String, prfBuilt :: !Int, prfChanged :: !Int, prfVisited :: !Int, prfDepends :: [[Int]], prfExecution :: !Seconds}

-- | Eliminate all errors from the database, pretending they don't exist
-- resultsOnly :: Map.HashMap Id (Key, Status) -> Map.HashMap Id (Key, Result (Either BS.ByteString Value))
resultsOnly :: [(Key, Status)] -> KeyMap Result
resultsOnly mp = mapKeyMap (\r ->
      r{resultDeps = mapResultDeps (filterKeySet (isJust . flip lookupKeyMap keep)) $ resultDeps r}
    ) keep
    where
        keep = fromListKeyMap $ mapMaybe (traverse getResult) mp

-- | Given a map of representing a dependency order (with a show for error messages), find an ordering for the items such
--   that no item points to an item before itself.
--   Raise an error if you end up with a cycle.
--
-- Algorithm:
--    Divide everyone up into those who have no dependencies [Id]
--    And those who depend on a particular Id, Dep :-> Maybe [(Key,[Dep])]
--    Where d :-> Just (k, ds), k depends on firstly d, then remaining on ds
--    For each with no dependencies, add to list, then take its dep hole and
--    promote them either to Nothing (if ds == []) or into a new slot.
--    k :-> Nothing means the key has already been freed
dependencyOrder :: (Key -> String) -> [(Key, [Key])] -> [Key]
dependencyOrder shw status =
  f (map fst noDeps) $
    mapKeyMap Just $
      fromListWithKeyMap (++)
        [(d, [(k,ds)]) | (k,d:ds) <- hasDeps]
    where
        (noDeps, hasDeps) = partition (null . snd) status

        f [] mp | null bad = []
                | otherwise = error $ unlines $
                    "Internal invariant broken, database seems to be cyclic" :
                    map ("    " ++) bad ++
                    ["... plus " ++ show (length badOverflow) ++ " more ..." | not $ null badOverflow]
            where (bad,badOverflow) = splitAt 10 [shw i | (i, Just _) <- toListKeyMap mp]

        f (x:xs) mp = x : f (now++xs) later
            where free = fromMaybe [] $ lookupDefaultKeyMap (Just []) x mp
                  (now,later) = foldl' g ([], insertKeyMap x Nothing mp) free

        g (free, mp) (k, []) = (k:free, mp)
        g (free, mp) (k, d:ds) = case lookupDefaultKeyMap (Just []) d mp of
            Nothing   -> g (free, mp) (k, ds)
            Just todo -> (free, insertKeyMap d (Just $ (k,ds) : todo) mp)

prepareForDependencyOrder :: Database -> IO (KeyMap Result)
prepareForDependencyOrder db = do
    current <- readTVarIO $ databaseStep db
    insertKeyMap (newKey "alwaysRerun") (alwaysRerunResult current) .  resultsOnly
        <$> getDatabaseValues db

-- | Returns a list of profile entries, and a mapping linking a non-error Id to its profile entry
toReport :: Database -> IO ([ProfileEntry], KeyMap Int)
toReport db = do
    status <- prepareForDependencyOrder db
    let order = dependencyOrder show
                $ map (second (toListKeySet . getResultDepsDefault (singletonKeySet $ newKey "alwaysRerun") . resultDeps))
                $ toListKeyMap status
        ids = fromListKeyMap $ zip order [0..]

        steps = let xs = nubOrd $ concat [[resultChanged, resultBuilt, resultVisited] | Result{..} <- elemsKeyMap status]

                in Map.fromList $ zip (sortBy (flip compare) xs) [0..]

        f k Result{..} = ProfileEntry
            {prfName = show k
            ,prfBuilt = fromStep resultBuilt
            ,prfVisited = fromStep resultVisited
            ,prfChanged = fromStep resultChanged
            ,prfDepends = map pure $ elemsKeyMap $ restrictKeysKeyMap ids $ getResultDepsDefault (singletonKeySet $ newKey "alwaysRerun") resultDeps
            ,prfExecution = resultExecution
            }
            where fromStep i = fromJust $ Map.lookup i steps
    pure ([maybe (error "toReport") (f i) $ lookupKeyMap i status | i <- order], ids)

alwaysRerunResult :: Step -> Result
alwaysRerunResult current = Result (Value $ toDyn "<alwaysRerun>") (Step 0) (Step 0) current (ResultDeps mempty) 0 mempty

generateHTML :: Maybe [Int] -> [ProfileEntry] -> IO LBS.ByteString
generateHTML dirtyKeys xs = do
    report <- readDataFileHTML "profile.html"
    let f "data/profile-data.js" = pure $ LBS.pack $ "var profile =\n" ++ generateJSONProfile xs
        f "data/build-data.js" = pure $ LBS.pack $ "var build =\n" ++ generateJSONBuild dirtyKeys
        f other = error other
    runTemplate f report

generateJSONBuild :: Maybe [Int] -> String
generateJSONBuild (Just dirtyKeys) = jsonList [jsonList (map show dirtyKeys)]
generateJSONBuild Nothing          = jsonList []

generateJSONProfile :: [ProfileEntry] -> String
generateJSONProfile = jsonListLines . map showEntry
    where
        showEntry ProfileEntry{..} = jsonList $
            [show prfName
            ,showTime prfExecution
            ,show prfBuilt
            ,show prfChanged
            ,show prfVisited
            ] ++
            [show prfDepends | not (null prfDepends)]
        showTime x = if '.' `elem` y then dropWhileEnd (== '.') $ dropWhileEnd (== '0') y else y
            where y = showDP 4 x

jsonListLines :: [String] -> String
jsonListLines xs = "[" ++ intercalate "\n," xs ++ "\n]"

jsonList :: [String] -> String
jsonList xs = "[" ++ intercalate "," xs ++ "]"

-- Very hard to abstract over TH, so we do it with CPP
#ifdef FILE_EMBED
#define FILE(x) (pure (LBS.fromStrict $(embedFile =<< runIO (x))))
#else
#define FILE(x) (LBS.readFile =<< (x))
#endif

libraries :: [(String, IO LBS.ByteString)]
libraries =
    [("jquery.js",            FILE(JQuery.file))
    ,("jquery.dgtable.js",    FILE(DGTable.file))
    ,("jquery.flot.js",       FILE(Flot.file Flot.Flot))
    ,("jquery.flot.stack.js", FILE(Flot.file Flot.FlotStack))
    ]


-- | Template Engine. Perform the following replacements on a line basis:
--
-- * <script src="foo"></script> ==> <script>[[foo]]</script>
--
-- * <link href="foo" rel="stylesheet" type="text/css" /> ==> <style type="text/css">[[foo]]</style>
runTemplate :: (FilePath -> IO LBS.ByteString) -> LBS.ByteString -> IO LBS.ByteString
runTemplate ask = lbsMapLinesIO f
    where
        link = LBS.pack "<link href=\""
        script = LBS.pack "<script src=\""

        f x | Just file <- LBS.stripPrefix script y = do res <- grab file; pure $ LBS.pack "<script>\n" `LBS.append` res `LBS.append` LBS.pack "\n</script>"
            | Just file <- LBS.stripPrefix link y = do res <- grab file; pure $ LBS.pack "<style type=\"text/css\">\n" `LBS.append` res `LBS.append` LBS.pack "\n</style>"
            | otherwise = pure x
            where
                y = LBS.dropWhile isSpace x
                grab = asker . takeWhile (/= '\"') . LBS.unpack

        asker o@(splitFileName -> ("lib/",x)) =
            case lookup x libraries of
                Nothing  -> error $ "Template library, unknown library: " ++ o
                Just act -> act

        asker "shake.js" = readDataFileHTML "shake.js"
        asker "data/metadata.js" = do
            time <- getCurrentTime
            pure $ LBS.pack $
                "var version = \"0\"" ++
                "\nvar generated = " ++ iso8601Show time
        asker x = ask x

-- Perform a mapM on each line and put the result back together again
lbsMapLinesIO :: (LBS.ByteString -> IO LBS.ByteString) -> LBS.ByteString -> IO LBS.ByteString
-- If we do the obvious @fmap LBS.unlines . mapM f@ then all the monadic actions are run on all the lines
-- before it starts producing the lazy result, killing streaming and having more stack usage.
-- The real solution (albeit with too many dependencies for something small) is a streaming library,
-- but a little bit of unsafePerformIO does the trick too.
lbsMapLinesIO f = pure . LBS.unlines . map (unsafePerformIO . f) . LBS.lines
