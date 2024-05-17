{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module OutlineTests (tests) where

import           Config
import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Development.IDE.GHC.Compat      (GhcVersion (..), ghcVersion)
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
import           Test.Hls.FileSystem         (file, text)
import           Test.Tasty
import           Test.Tasty.HUnit

testSymbols :: (HasCallStack) => TestName -> FilePath -> [Text] -> [DocumentSymbol] -> TestTree
testSymbols testName path content expectedSymbols =
  testCase testName $ runWithDummyPlugin (mkIdeTestFs [file path (text $ T.unlines content)]) $ do
    docId <- openDoc path "haskell"
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right expectedSymbols

testSymbolsA :: (HasCallStack) => TestName -> [Text] -> [DocumentSymbol] -> TestTree
testSymbolsA testName content expectedSymbols =
  testSymbols testName "A.hs" content expectedSymbols

tests :: TestTree
tests =
  testGroup
    "outline"
    [ testSymbolsA
        "type class:"
        ["module A where", "class A a where a :: a -> Bool"]
        [ moduleSymbol
            "A"
            (R 0 7 0 8)
            [ classSymbol
                "A a"
                (R 1 0 1 30)
                [docSymbol' "a" SymbolKind_Method (R 1 16 1 30) (R 1 16 1 17)]
            ]
        ],
      testSymbolsA
        "type class instance "
        ["class A a where", "instance A () where"]
        [ classSymbol "A a" (R 0 0 0 15) [],
          docSymbol "A ()" SymbolKind_Interface (R 1 0 1 19)
        ],
      testSymbolsA "type family" ["{-# language TypeFamilies #-}", "type family A"] [docSymbolD "A" "type family" SymbolKind_Function (R 1 0 1 13)],
      testSymbolsA
        "type family instance "
        ["{-# language TypeFamilies #-}", "type family A a", "type instance A () = ()"]
        [ docSymbolD "A a" "type family" SymbolKind_Function (R 1 0 1 15),
          docSymbol "A ()" SymbolKind_Interface (R 2 0 2 23)
        ],
      testSymbolsA "data family" ["{-# language TypeFamilies #-}", "data family A"] [docSymbolD "A" "data family" SymbolKind_Function (R 1 0 1 (if ghcVersion >= GHC910 then 13 else 11))],
      testSymbolsA
        "data family instance "
        ["{-# language TypeFamilies #-}", "data family A a", "data instance A () = A ()"]
        [ docSymbolD "A a" "data family" SymbolKind_Function (R 1 0 1 (if ghcVersion >= GHC910 then 15 else 11)),
          docSymbol "A ()" SymbolKind_Interface (R 2 0 2 25)
        ],
      testSymbolsA "constant" ["a = ()"] [docSymbol "a" SymbolKind_Function (R 0 0 0 6)],
      testSymbolsA "pattern" ["Just foo = Just 21"] [docSymbol "Just foo" SymbolKind_Function (R 0 0 0 18)],
      testSymbolsA "pattern with type signature" ["{-# language ScopedTypeVariables #-}", "a :: () = ()"] [docSymbol "a :: ()" SymbolKind_Function (R 1 0 1 12)],
      testSymbolsA "function" ["a _x = ()"] [docSymbol "a" SymbolKind_Function (R 0 0 0 9)],
      testSymbolsA "type synonym" ["type A = Bool"] [docSymbol' "A" SymbolKind_TypeParameter (R 0 0 0 13) (R 0 5 0 6)],
      testSymbolsA "datatype" ["data A = C"] [docSymbolWithChildren "A" SymbolKind_Struct (R 0 0 0 10) [docSymbol "C" SymbolKind_Constructor (R 0 9 0 10)]],
      testSymbolsA
        "record fields"
        ["data A = B {", "  x :: Int", "  , y :: Int}"]
        [ docSymbolWithChildren
            "A"
            SymbolKind_Struct
            (R 0 0 2 13)
            [ docSymbolWithChildren'
                "B"
                SymbolKind_Constructor
                (R 0 9 2 13)
                (R 0 9 0 10)
                [ docSymbol "x" SymbolKind_Field (R 1 2 1 3),
                  docSymbol "y" SymbolKind_Field (R 2 4 2 5)
                ]
            ]
        ],
      testSymbolsA
        "import"
        ["import Data.Maybe ()"]
        [ docSymbolWithChildren
            "imports"
            SymbolKind_Module
            (R 0 0 0 20)
            [ docSymbol "import Data.Maybe" SymbolKind_Module (R 0 0 0 20)
            ]
        ],
      testSymbolsA
        "multiple import"
        ["", "import Data.Maybe ()", "", "import Control.Exception ()", ""]
        [ docSymbolWithChildren
            "imports"
            SymbolKind_Module
            (R 1 0 3 27)
            [ docSymbol "import Data.Maybe" SymbolKind_Module (R 1 0 1 20),
              docSymbol "import Control.Exception" SymbolKind_Module (R 3 0 3 27)
            ]
        ],
      testSymbolsA
        "foreign import"
        [ "{-# language ForeignFunctionInterface #-}",
          "foreign import ccall \"a\" a :: Int"
        ]
        [docSymbolD "a" "import" SymbolKind_Object (R 1 0 1 33)],
      testSymbolsA
        "foreign export"
        [ "{-# language ForeignFunctionInterface #-}",
          "foreign export ccall odd :: Int -> Bool"
        ]
        [docSymbolD "odd" "export" SymbolKind_Object (R 1 0 1 39)]
    ]
  where
    docSymbol name kind loc =
      DocumentSymbol name Nothing kind Nothing Nothing loc loc Nothing
    docSymbol' name kind loc selectionLoc =
      DocumentSymbol name Nothing kind Nothing Nothing loc selectionLoc Nothing
    docSymbolD name detail kind loc =
      DocumentSymbol name (Just detail) kind Nothing Nothing loc loc Nothing
    docSymbolWithChildren name kind loc cc =
      DocumentSymbol name Nothing kind Nothing Nothing loc loc (Just cc)
    docSymbolWithChildren' name kind loc selectionLoc cc =
      DocumentSymbol name Nothing kind Nothing Nothing loc selectionLoc (Just cc)
    moduleSymbol name loc cc =
      DocumentSymbol
        name
        Nothing
        SymbolKind_File
        Nothing
        Nothing
        (R 0 0 maxBound 0)
        loc
        (Just cc)
    classSymbol name loc cc =
      DocumentSymbol
        name
        (Just "class")
        SymbolKind_Interface
        Nothing
        Nothing
        loc
        loc
        (Just cc)
