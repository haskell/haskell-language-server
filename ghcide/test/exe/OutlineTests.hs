
module OutlineTests (tests) where

import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types hiding (SemanticTokenAbsolute (..),
                                              SemanticTokenRelative (..),
                                              SemanticTokensEdit (..), mkRange)
import           Language.LSP.Test
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtils

tests :: TestTree
tests = testGroup
  "outline"
  [ testSessionWait "type class" $ do
    let source = T.unlines ["module A where", "class A a where a :: a -> Bool"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [ moduleSymbol
          "A"
          (R 0 7 0 8)
          [ classSymbol "A a"
                        (R 1 0 1 30)
                        [docSymbol' "a" SymbolKind_Method (R 1 16 1 30) (R 1 16 1 17)]
          ]
      ]
  , testSessionWait "type class instance " $ do
    let source = T.unlines ["class A a where", "instance A () where"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [ classSymbol "A a" (R 0 0 0 15) []
      , docSymbol "A ()" SymbolKind_Interface (R 1 0 1 19)
      ]
  , testSessionWait "type family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "type family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right [docSymbolD "A" "type family" SymbolKind_Function (R 1 0 1 13)]
  , testSessionWait "type family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "type family A a"
          , "type instance A () = ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [ docSymbolD "A a"   "type family" SymbolKind_Function     (R 1 0 1 15)
      , docSymbol "A ()" SymbolKind_Interface (R 2 0 2 23)
      ]
  , testSessionWait "data family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "data family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right [docSymbolD "A" "data family" SymbolKind_Function (R 1 0 1 11)]
  , testSessionWait "data family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "data family A a"
          , "data instance A () = A ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [ docSymbolD "A a"   "data family" SymbolKind_Function     (R 1 0 1 11)
      , docSymbol "A ()" SymbolKind_Interface (R 2 0 2 25)
      ]
  , testSessionWait "constant" $ do
    let source = T.unlines ["a = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [docSymbol "a" SymbolKind_Function (R 0 0 0 6)]
  , testSessionWait "pattern" $ do
    let source = T.unlines ["Just foo = Just 21"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [docSymbol "Just foo" SymbolKind_Function (R 0 0 0 18)]
  , testSessionWait "pattern with type signature" $ do
    let source = T.unlines ["{-# language ScopedTypeVariables #-}", "a :: () = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [docSymbol "a :: ()" SymbolKind_Function (R 1 0 1 12)]
  , testSessionWait "function" $ do
    let source = T.unlines ["a _x = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right [docSymbol "a" SymbolKind_Function (R 0 0 0 9)]
  , testSessionWait "type synonym" $ do
    let source = T.unlines ["type A = Bool"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [docSymbol' "A" SymbolKind_TypeParameter (R 0 0 0 13) (R 0 5 0 6)]
  , testSessionWait "datatype" $ do
    let source = T.unlines ["data A = C"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [ docSymbolWithChildren "A"
                              SymbolKind_Struct
                              (R 0 0 0 10)
                              [docSymbol "C" SymbolKind_Constructor (R 0 9 0 10)]
      ]
  , testSessionWait "record fields" $ do
    let source = T.unlines ["data A = B {", "  x :: Int", "  , y :: Int}"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [ docSymbolWithChildren "A" SymbolKind_Struct (R 0 0 2 13)
          [ docSymbolWithChildren' "B" SymbolKind_Constructor (R 0 9 2 13) (R 0 9 0 10)
            [ docSymbol "x" SymbolKind_Field (R 1 2 1 3)
            , docSymbol "y" SymbolKind_Field (R 2 4 2 5)
            ]
          ]
      ]
  , testSessionWait "import" $ do
    let source = T.unlines ["import Data.Maybe ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [docSymbolWithChildren "imports"
                             SymbolKind_Module
                             (R 0 0 0 20)
                             [ docSymbol "import Data.Maybe" SymbolKind_Module (R 0 0 0 20)
                             ]
      ]
  , testSessionWait "multiple import" $ do
    let source = T.unlines ["", "import Data.Maybe ()", "", "import Control.Exception ()", ""]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right
      [docSymbolWithChildren "imports"
                             SymbolKind_Module
                             (R 1 0 3 27)
                             [ docSymbol "import Data.Maybe" SymbolKind_Module (R 1 0 1 20)
                             , docSymbol "import Control.Exception" SymbolKind_Module (R 3 0 3 27)
                             ]
      ]
  , testSessionWait "foreign import" $ do
    let source = T.unlines
          [ "{-# language ForeignFunctionInterface #-}"
          , "foreign import ccall \"a\" a :: Int"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right [docSymbolD "a" "import" SymbolKind_Object (R 1 0 1 33)]
  , testSessionWait "foreign export" $ do
    let source = T.unlines
          [ "{-# language ForeignFunctionInterface #-}"
          , "foreign export ccall odd :: Int -> Bool"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Right [docSymbolD "odd" "export" SymbolKind_Object (R 1 0 1 39)]
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
  moduleSymbol name loc cc = DocumentSymbol name
                                            Nothing
                                            SymbolKind_File
                                            Nothing
                                            Nothing
                                            (R 0 0 maxBound 0)
                                            loc
                                            (Just cc)
  classSymbol name loc cc = DocumentSymbol name
                                           (Just "class")
                                           SymbolKind_Interface
                                           Nothing
                                           Nothing
                                           loc
                                           loc
                                           (Just cc)
