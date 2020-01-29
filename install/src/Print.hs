module Print where

import           Development.Shake
import           Development.Shake.Command
import           Control.Monad.IO.Class
import           Data.List                                ( dropWhileEnd
                                                          , dropWhile
                                                          )
import           Data.Char                                ( isSpace )

-- | lift putStrLn to MonadIO
printLine :: MonadIO m => String -> m ()
printLine = liftIO . putStrLn

-- | print a line prepended with 4 spaces
printLineIndented :: MonadIO m => String -> m ()
printLineIndented = printLine . ("    " ++)

embedInStars :: String -> String
embedInStars str =
  let starsLine = "\n" <> replicate 80 '*' <> "\n"
  in  starsLine <> str <> starsLine

printInStars :: MonadIO m => String -> m ()
printInStars = liftIO . putStrLn . embedInStars


-- | Trim whitespace of both ends of a string
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Trim the whitespace of the stdout of a command
trimmedStdout :: Stdout String -> String
trimmedStdout (Stdout s) = trim s

type TargetDescription = (String, String)

-- | Number of spaces the target name including whitespace should have.
-- At least twenty, maybe more if target names are long. At most the length of the longest target plus five.
space :: [TargetDescription] -> Int
space phonyTargets = maximum (20 : map ((+ 5) . length . fst) phonyTargets)

-- | Show a target.
-- Concatenates the target with its help message and inserts whitespace between them.
showTarget :: Int -> TargetDescription -> String
showTarget spaces (target, msg) =
  target ++ replicate (spaces - length target) ' ' ++ msg
