#!/usr/bin/env stack
{- stack
  runghc
  --stack-yaml=install/shake.yaml
  --package hie-install
-}
{- cabal:
build-depends:
    base
  , hie-install
-}
-- call as:
-- * `cabal v2-run install.hs --project-file install/shake.project <target>`
-- * `stack install.hs <target>`

-- TODO: set `shake.project` in cabal-config above, when supported
-- (see https://github.com/haskell/cabal/issues/6353)

import HieInstall (defaultMain)

main = defaultMain
