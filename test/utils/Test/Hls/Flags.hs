{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | Module for disabling tests if their plugins are disabled
-- DEPRECATED: To be removed when all plugin tests are in their own packages
module Test.Hls.Flags where

import           Test.Hls (TestTree, ignoreTestBecause)

-- * Plugin dependent tests

-- | Disable test unless the eval flag is set
requiresEvalPlugin            :: TestTree -> TestTree
#if eval
requiresEvalPlugin            = id
#else
requiresEvalPlugin            = ignoreTestBecause "Eval plugin disabled"
#endif

-- * Formatters
-- | Disable test unless the floskell flag is set
requiresFloskellPlugin        :: TestTree -> TestTree
#if floskell
requiresFloskellPlugin        = id
#else
requiresFloskellPlugin        = ignoreTestBecause "Floskell plugin disabled"
#endif

-- | Disable test unless the fourmolu flag is set
requiresFourmoluPlugin        :: TestTree -> TestTree
#if fourmolu
requiresFourmoluPlugin        = id
#else
requiresFourmoluPlugin        = ignoreTestBecause "Fourmolu plugin disabled"
#endif

-- | Disable test unless the ormolu flag is set
requiresOrmoluPlugin          :: TestTree -> TestTree
#if ormolu
requiresOrmoluPlugin          = id
#else
requiresOrmoluPlugin          = ignoreTestBecause "Ormolu plugin disabled"
#endif
