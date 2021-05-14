{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.RunMetaprogramSpec where

import  Utils
import  Test.Hspec
import  Wingman.Types


spec :: Spec
spec = do
  let metaTest l c f =
#if __GLASGOW_HASKELL__ >= 808
        goldenTest RunMetaprogram "" l c f
#else
        pure ()
#endif

#if __GLASGOW_HASKELL__ >= 808
  describe "beginMetaprogram" $ do
    goldenTest BeginMetaprogram ""  1  7 "MetaBegin.hs"
#endif

  describe "golden" $ do
    metaTest  6 11 "MetaMaybeAp.hs"
    metaTest  2 32 "MetaBindOne.hs"
    metaTest  2 32 "MetaBindAll.hs"
    metaTest  2 13 "MetaTry.hs"
    metaTest  2 74 "MetaChoice.hs"

