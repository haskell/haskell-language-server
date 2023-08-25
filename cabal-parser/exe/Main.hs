module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Text.IO                     as T
import           Options
import           Options.Applicative
import           Prettyprinter
import           System.Directory                 (canonicalizePath,
                                                   doesDirectoryExist,
                                                   listDirectory)
import           System.FilePath
import           System.IO
import           Text.Cabal.Parser
import           Text.Megaparsec                  (errorBundlePretty)

main :: IO ()
main = do
  options <- execParser opts
  case optCommand options of
    Parse parseOpts -> do
      analytics <- flip execStateT defParseAnalytics $ forM_ (parseFiles parseOpts) $ \fileTarget' -> do
        fileTarget <- liftIO $ canonicalizePath fileTarget'
        isDirectory <- liftIO $ doesDirectoryExist fileTarget

        if isDirectory
          then do
            targets <- liftIO $ listDirectory fileTarget
            forM_ targets $ \target ->
              parseFileAndReport parseOpts (fileTarget </> target)
          else do
            parseFileAndReport parseOpts fileTarget

      putStrLn $ showAnalytics analytics

parseFileAndReport :: MonadIO m => ParseOptions -> FilePath -> StateT ParseAnalytics m ()
parseFileAndReport parseOpts cabalFile = do
  cabalFileContents <- liftIO $ T.readFile cabalFile
  liftIO $ putStr $ "Parsing \"" <> cabalFile <> "\""
  liftIO $ hFlush stdout
  case parseCabalFile cabalFile cabalFileContents of
    Left err -> do
      addFailure
      liftIO $ putStrLn $ " failed"
      when (not $ silent parseOpts) $ do
        liftIO $ putStrLn $ errorBundlePretty err
    Right ast -> do
      addSuccess
      liftIO $ putStrLn $ " succeeded"
      when (showAst parseOpts) $ do
        liftIO $ putStrLn $ show $ pretty ast

data ParseAnalytics = ParseAnalytics
  { parsedSuccessfully :: !Int
  , parsedFailed       :: !Int
  , parsedNum          :: !Int
  }
  deriving (Show, Eq, Ord)

defParseAnalytics :: ParseAnalytics
defParseAnalytics =
  ParseAnalytics
    { parsedSuccessfully = 0
    , parsedFailed = 0
    , parsedNum = 0
    }

addSuccess :: (Monad m) => StateT ParseAnalytics m ()
addSuccess = do
  modify'
    ( \pa ->
        pa
          { parsedSuccessfully = parsedSuccessfully pa + 1
          , parsedNum = parsedNum pa + 1
          }
    )

addFailure :: (Monad m) => StateT ParseAnalytics m ()
addFailure = do
  modify'
    ( \pa ->
        pa
          { parsedFailed = parsedFailed pa + 1
          , parsedNum = parsedNum pa + 1
          }
    )

showAnalytics :: ParseAnalytics -> String
showAnalytics a
  | parsedFailed a == 0 && parsedSuccessfully a > 0 = "Successfully parsed all cabal files (" <> show (parsedNum a) <> ")"
  | otherwise = "Parsed " <> show (parsedSuccessfully a) <> " out of " <> show (parsedNum a) <> " cabal files"
