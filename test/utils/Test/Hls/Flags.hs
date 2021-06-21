{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | Module for disabling tests if their plugins are disabled
module Test.Hls.Flags where

import           Test.Hls (TestTree, ignoreTestBecause)

-- * Plugin dependent tests

-- | Disable test unless the class flag is set
requiresClassPlugin           :: TestTree -> TestTree
#if class
requiresClassPlugin           = id
#else
requiresClassPlugin           = ignoreTestBecause "Class plugin disabled"
#endif

-- | Disable test unless the haddockComments flag is set
requiresHaddockCommentsPlugin :: TestTree -> TestTree
#if haddockComments
requiresHaddockCommentsPlugin = id
#else
requiresHaddockCommentsPlugin = ignoreTestBecause "HaddockComments plugin disabled"
#endif

-- | Disable test unless the eval flag is set
requiresEvalPlugin            :: TestTree -> TestTree
#if eval
requiresEvalPlugin            = id
#else
requiresEvalPlugin            = ignoreTestBecause "Eval plugin disabled"
#endif

-- | Disable test unless the importLens flag is set
requiresImportLensPlugin      :: TestTree -> TestTree
#if importLens
requiresImportLensPlugin      = id
#else
requiresImportLensPlugin      = ignoreTestBecause "ImportLens plugin disabled"
#endif

-- | Disable test unless the retrie flag is set
requiresRetriePlugin          :: TestTree -> TestTree
#if retrie
requiresRetriePlugin          = id
#else
requiresRetriePlugin          = ignoreTestBecause "Retrie plugin disabled"
#endif

-- | Disable test unless the tactic flag is set
requiresTacticPlugin          :: TestTree -> TestTree
#if tactic
requiresTacticPlugin          = id
#else
requiresTacticPlugin          = ignoreTestBecause "Tactic plugin disabled"
#endif

-- | Disable test unless the hlint flag is set
requiresHlintPlugin           :: TestTree -> TestTree
#if hlint
requiresHlintPlugin           = id
#else
requiresHlintPlugin           = ignoreTestBecause "Hlint plugin disabled"
#endif

-- | Disable test unless the moduleName flag is set
requiresModuleNamePlugin      :: TestTree -> TestTree
#if moduleName
requiresModuleNamePlugin      = id
#else
requiresModuleNamePlugin      = ignoreTestBecause "ModuleName plugin disabled"
#endif

-- | Disable test unless the pragmas flag is set
requiresPragmasPlugin         :: TestTree -> TestTree
#if pragmas
requiresPragmasPlugin         = id
#else
requiresPragmasPlugin         = ignoreTestBecause "Pragmas plugin disabled"
#endif

-- | Disable test unless the splice flag is set
requiresSplicePlugin          :: TestTree -> TestTree
#if splice
requiresSplicePlugin          = id
#else
requiresSplicePlugin          = ignoreTestBecause "Splice plugin disabled"
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

-- | Disable test unless the stylishHaskell flag is set
requiresStylishHaskellPlugin  :: TestTree -> TestTree
#if stylishHaskell
requiresStylishHaskellPlugin  = id
#else
requiresStylishHaskellPlugin  = ignoreTestBecause "StylishHaskell plugin disabled"
#endif

-- | Disable test unless the brittany flag is set
requiresBrittanyPlugin        :: TestTree -> TestTree
#if brittany
requiresBrittanyPlugin        = id
#else
requiresBrittanyPlugin        = ignoreTestBecause "Brittany plugin disabled"
#endif

