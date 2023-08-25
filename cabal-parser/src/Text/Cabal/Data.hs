{-# LANGUAGE OverloadedStrings #-}

module Text.Cabal.Data where

import           Data.Map         (Map)
import qualified Data.Map         as Map
import qualified Data.Text        as T
import           Text.Cabal.Types
import           Text.Cabal.Value

{- | Top level keywords of a cabal file and the parsers
  to be used to parse their corresponding values.
-}
cabalKeywords :: Map T.Text (Bool -> Parser [ValueItem])
cabalKeywords =
  Map.fromList
    [ ("cabal-version:", defaultValueParser)
    , ("name:", defaultValueParser)
    , ("version:", defaultValueParser)
    , ("build-type:", defaultValueParser)
    , ("license:", defaultValueParser)
    , ("license-file:", defaultValueParser)
    , ("license-files:", defaultValueParser)
    , ("copyright:", defaultValueParser)
    , ("author:", defaultValueParser)
    , ("maintainer:", defaultValueParser)
    , ("stability:", defaultValueParser)
    , ("homepage:", defaultValueParser)
    , ("bug-reports:", defaultValueParser)
    , ("package-url:", defaultValueParser)
    , ("synopsis:", defaultValueParser)
    , ("description:", defaultValueParser)
    , ("category:", defaultValueParser)
    , ("tested-with:", defaultValueParser)
    , ("data-files:", defaultValueParser)
    , ("data-dir:", defaultValueParser)
    , ("extra-source-files:", defaultValueParser)
    , ("extra-doc-files:", defaultValueParser)
    , ("extra-tmp-files:", defaultValueParser)
    ]

{- | Map, containing all stanzas in a cabal file as keys
  and lists of their possible nested keywords as values
  and the parsers to use to parse the keywords' corresponding values.
-}
stanzaKeywordMap :: Map T.Text (Map T.Text (Bool -> Parser [ValueItem]))
stanzaKeywordMap =
  Map.fromList
    [
      ( "library"
      , Map.fromList $
          [ ("exposed-modules:", moduleParser)
          , ("virtual-modules:", moduleParser)
          , ("exposed:", defaultValueParser)
          , ("import:", defaultValueParser)
          , ("visibility:", defaultValueParser)
          , ("reexported-modules:", defaultValueParser)
          , ("signatures:", defaultValueParser)
          , ("other-modules:", moduleParser)
          ]
            ++ libExecTestBenchCommons
      )
    ,
      ( "executable"
      , Map.fromList $
          [ ("main-is:", defaultValueParser)
          , ("import:", defaultValueParser)
          , ("scope:", defaultValueParser)
          , ("other-modules:", moduleParser)
          ]
            ++ libExecTestBenchCommons
      )
    ,
      ( "test-suite"
      , Map.fromList $
          [ ("type:", defaultValueParser)
          , ("import:", defaultValueParser)
          , ("main-is:", defaultValueParser)
          , ("other-modules:", moduleParser)
          ]
            ++ libExecTestBenchCommons
      )
    ,
      ( "benchmark"
      , Map.fromList $
          [ ("type:", defaultValueParser)
          , ("import:", defaultValueParser)
          , ("main-is:", defaultValueParser)
          , ("other-modules:", moduleParser)
          ]
            ++ libExecTestBenchCommons
      )
    ,
      ( "foreign-library"
      , Map.fromList
          [ ("type:", defaultValueParser)
          , ("options:", defaultValueParser)
          , ("import:", defaultValueParser)
          , ("mod-def-file:", defaultValueParser)
          , ("lib-version-info:", defaultValueParser)
          , ("lib-version-linux:", defaultValueParser)
          ]
      )
    ,
      ( "flag"
      , Map.fromList
          [ ("description:", defaultValueParser)
          , ("default:", defaultValueParser)
          , ("manual:", defaultValueParser)
          , ("lib-def-file:", defaultValueParser)
          , ("lib-version-info:", defaultValueParser)
          , ("lib-version-linux:", defaultValueParser)
          ]
      )
    ,
      ( "source-repository"
      , Map.fromList
          [
            ( "type:"
            , defaultValueParser
            )
          , ("location:", defaultValueParser)
          , ("module:", defaultValueParser)
          , ("branch:", defaultValueParser)
          , ("tag:", defaultValueParser)
          , ("subdir:", defaultValueParser)
          ]
      )
    ,
      ( "custom-setup"
      , Map.fromList
          [("setup-depends:", defaultValueParser)]
      )
    ,
      ( "common"
      , Map.fromList
          libExecTestBenchCommons
      )
    ]
 where
  libExecTestBenchCommons =
    [ ("autogen-modules:", moduleParser)
    , ("build-tools:", defaultValueParser)
    , ("build-depends:", defaultValueParser)
    , ("hs-source-dirs:", filepathParser)
    , ("extensions:", defaultValueParser)
    , ("default-extensions:", defaultValueParser)
    , ("other-extensions:", defaultValueParser)
    , ("default-language:", defaultValueParser)
    , ("other-languages:", defaultValueParser)
    , ("build-tool-depends:", defaultValueParser)
    , ("buildable:", defaultValueParser)
    , ("ghc-options:", defaultValueParser)
    , ("ghc-prof-options:", defaultValueParser)
    , ("ghc-shared-options:", defaultValueParser)
    , ("ghcjs-options:", defaultValueParser)
    , ("ghcjs-prof-options:", defaultValueParser)
    , ("ghcjs-shared-options:", defaultValueParser)
    , ("includes:", defaultValueParser)
    , ("install-includes:", defaultValueParser)
    , ("include-dirs:", defaultValueParser)
    , ("c-sources:", defaultValueParser)
    , ("cxx-sources:", defaultValueParser)
    , ("asm-sources:", defaultValueParser)
    , ("cmm-sources:", defaultValueParser)
    , ("js-sources:", defaultValueParser)
    , ("extra-libraries:", defaultValueParser)
    , ("extra-ghci-libraries:", defaultValueParser)
    , ("extra-bundled-libraries:", defaultValueParser)
    , ("extra-lib-dirs:", defaultValueParser)
    , ("cc-options:", defaultValueParser)
    , ("cpp-options:", defaultValueParser)
    , ("cxx-options:", defaultValueParser)
    , ("cmm-options:", defaultValueParser)
    , ("asm-options:", defaultValueParser)
    , ("ld-options:", defaultValueParser)
    , ("pkgconfig-depends:", defaultValueParser)
    , ("frameworks:", defaultValueParser)
    , ("extra-framework-dirs:", defaultValueParser)
    , ("mixins:", defaultValueParser)
    ]

{- | Returns a list of all possible keywords that may occur in a cabal file
regardless of which contexts these keywords occur in
-}
allKeywords :: Map T.Text (Bool -> Parser [ValueItem])
allKeywords =
  Map.unions $ [cabalKeywords] <> Map.elems stanzaKeywordMap
