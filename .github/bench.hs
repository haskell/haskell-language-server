#!/usr/bin/env cabal
{- cabal:
build-depends: base, split, text, cassava, vector, bytestring, directory, filepath, containers, extra
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
module Main where

import           Control.Monad        (forM_)
import qualified Data.ByteString.Lazy
import           Data.Csv             (FromNamedRecord (..), Header,
                                       decodeByName, decodeByNameWith, (.:))
import           Data.List.Extra      (stripSuffix)
import           Data.Map             (elems, intersectionWith, toList)
import qualified Data.Map
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import qualified Data.Text.IO
import qualified Data.Vector          (Vector, toList)
import           System.Directory     (listDirectory)
import           System.Environment   (getArgs)
import           System.FilePath      ((</>))

data CsvRecord = CsvRecord { name:: !String, success :: !T.Text, firstTime :: !Double, avgTime :: !Double}
  deriving Show
instance FromNamedRecord CsvRecord where
    parseNamedRecord m = CsvRecord <$>
                         m .: "name" <*>
                         m .: "success" <*>
                         m .: "1stBuildT" <*>
                         m .: "avgPerRespT"


readCsv :: FilePath -> IO ([CsvRecord])
readCsv fp= do
  contents <- Data.ByteString.Lazy.readFile fp
  let Right csv = decodeByName contents :: Either String (Header,Data.Vector.Vector CsvRecord )
  pure $ Data.Vector.toList $ snd csv

readRts :: FilePath ->IO [(String,Double)]
-- right now we don't worry about int vs double metrics
readRts fp= do
  contents <- Data.Text.IO.readFile fp
  let contents' = T.unlines $ tail $ T.lines contents
  let contents'' = read $ T.unpack contents' ::[(String,String)]
  pure$ map (\(x,y) -> (x,read y)) contents''
readAllRts :: FilePath -> IO [(FilePath,[(String,Double)])]
readAllRts fp = do
  files <- listDirectory fp
  mapM (\x -> readRts (fp </> x) >>= \y -> pure (normalizeName x,y)) files
  where
    normalizeName :: String -> String
    normalizeName s = fromJust $ stripSuffix ".rts"  s

main :: IO ()
main = do
  (csvPath:rtsDir:_) <- getArgs
  csv <- readCsv csvPath
  let csvMap = Data.Map.fromList $ map (\x -> (map (\c -> if c == ' ' then '-' else c) $ name x,x)) csv
  rtsMap<- Data.Map.fromList `fmap` readAllRts rtsDir
  let measurements = intersectionWith (\x -> \y -> (x,y)) csvMap rtsMap

  forM_ (elems measurements) printResult
printResult :: (CsvRecord,[(String,Double)]) -> IO ()
printResult (csv,xs) = do
  putStrLn $ name csv
  putStrLn $ "\tSuccess: " ++ ( T.unpack $ success csv)
  if (T.unpack $ success csv) == "True" then do
    putStrLn $ "\t1st time: " ++ ( show $ firstTime csv)
    putStrLn $ "\tavg time: " ++ ( show $ avgTime csv)
    forM_ xs (\ (n,m) -> putStrLn $ "\t"++n ++ ": "++ (show m))
  else pure ()

