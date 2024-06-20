module Main where

import           Config
import           ConfigSchema
import           Format
import           FunctionalBadProject
import           HieBios
import           Progress
import           Test.Hls

main :: IO ()
main = defaultTestRunner $ testGroup "haskell-language-server"
    [ Config.tests
    , ConfigSchema.tests
    , ignoreInEnv [HostOS Windows] "Tests gets stuck in ci" $ Format.tests
    , FunctionalBadProject.tests
    , HieBios.tests
    , ignoreInEnv [HostOS Windows] "Tests gets stuck in ci" $ Progress.tests
    ]
