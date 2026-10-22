module Main where

import           Config
import           ConfigSchema
import           Format
import           FunctionalBadProject
import           HieBios
import           Progress
import           Test.Hls

main :: IO ()
main = defaultTestRunner $ dependentTestGroup "haskell-language-server" AllFinish
    [ testGroup "part 1"
      [Config.tests
      , ConfigSchema.tests]
    , testGroup "part 2"
      [ ignoreInEnv [HostOS Windows] "Tests gets stuck in ci" Format.tests
      , FunctionalBadProject.tests
      , HieBios.tests
      , ignoreInEnv [HostOS Windows] "Tests gets stuck in ci" Progress.tests
      ]
    ]
