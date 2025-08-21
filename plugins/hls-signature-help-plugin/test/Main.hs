{-# LANGUAGE QuasiQuotes #-}

import           Control.Exception                        (throw)
import           Control.Lens                             ((%~), (^.))
import           Data.Maybe                               (fromJust)
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Development.IDE.Plugin.Completions.Types (PosPrefixInfo (PosPrefixInfo))
import           Ide.Plugin.SignatureHelp                 (descriptor)
import qualified Language.LSP.Protocol.Lens               as L
import           Test.Hls
import           Test.Hls.FileSystem                      (VirtualFileTree,
                                                           directCradle, file,
                                                           mkVirtualFileTree,
                                                           text)
import           Text.Regex.TDFA                          ((=~))


main :: IO ()
main =
    defaultTestRunner $
        testGroup
            "signatureHelp"
            [ mkTest
                  "1 parameter"
                  [trimming|
                      f :: Int -> Int
                      f = _
                      x = f 1
                      ^^^^^^^^
                  |]
                  [ Nothing,
                    Nothing,
                    Nothing,
                    Nothing,
                    Nothing,
                    Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "2 parameters"
                  [trimming|
                      f :: Int -> Int -> Int
                      f = _
                      x = f 1 2
                          ^ ^^^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
                  ],
              mkTest
                  "3 parameters"
                  [trimming|
                      f :: Int -> Int -> Int -> Int
                      f = _
                      x = f 1 2 3
                          ^ ^ ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing, ParameterInformation (InR (19,22)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing, ParameterInformation (InR (19,22)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1)),
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing, ParameterInformation (InR (19,22)) Nothing]) (Just (InL 2))] (Just 0) (Just (InL 2))
                  ],
              mkTest
                  "parentheses"
                  [trimming|
                      f :: Int -> Int -> Int
                      f = _
                      x = (f 1) 2
                          ^^ ^^^^
                  |]
                  [ Nothing,
                    Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
                  ],
              mkTest
                  "newline"
                  [trimming|
                      f :: Int -> Int -> Int
                      f = _
                      x =
                        (
                        ^
                        f
                        ^
                        1
                        ^
                        )
                        ^
                        2
                        ^

                        ^
                  |]
                  [ Nothing,
                    Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1)),
                    Nothing
                  ],
              mkTest
                  "nested"
                  [trimming|
                      f :: Int -> Int -> Int
                      f = _
                      g :: Int -> Int
                      g = _
                      x = f (g 1) 2
                          ^^^^ ^^^^
                  |]
                  [ Nothing,
                    Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Nothing,
                    Just $ SignatureHelp [SignatureInformation "g :: Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "g :: Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
                  ],
              mkTest
                  "higher-order function"
                  [trimming|
                      f :: (Int -> Int) -> Int -> Int
                      f = _
                      x = f (+ 1) 2
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: (Int -> Int) -> Int -> Int" Nothing (Just [ParameterInformation (InR (6,16)) Nothing, ParameterInformation (InR (21,24)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "type constraint"
                  [trimming|
                      f :: (Num a) => a -> a -> a
                      f = _
                      x = f 1 2
                          ^ ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. Num a => a -> a -> a" Nothing (Just [ParameterInformation (InR (24,25)) Nothing, ParameterInformation (InR (29,30)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (16,23)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. Num a => a -> a -> a" Nothing (Just [ParameterInformation (InR (24,25)) Nothing, ParameterInformation (InR (29,30)) Nothing]) (Just (InL 1)), SignatureInformation "f :: Integer -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (16,23)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
                  ],
              mkTest
                  "type constraint with kind signatures"
                  [trimming|
                      x :: IO Bool
                      x = pure True
                           ^   ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "pure :: forall (f :: Type -> Type) a. Applicative f => a -> f a" (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nLift a value.\n\n\\[Documentation\\]\\(file://.*\\)\n\n\\[Source\\]\\(file://.*\\)\n\n") (Just [ParameterInformation (InR (55,56)) Nothing]) (Just (InL 0)), SignatureInformation "pure :: Bool -> IO Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nLift a value.\n\n\\[Documentation\\]\\(file://.*\\)\n\n\\[Source\\]\\(file://.*\\)\n\n") (Just [ParameterInformation (InR (8,12)) Nothing]) (Just (InL 0)), SignatureInformation "pure :: forall a. a -> IO a" (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nLift a value.\n\n\\[Documentation\\]\\(file://.*\\)\n\n\\[Source\\]\\(file://.*\\)\n\n") (Just [ParameterInformation (InR (18,19)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "2 type constraints"
                  [trimming|
                      f :: forall a. (Eq a, Num a) => a -> a -> a
                      f = _
                      x = f True
                          ^   ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. (Eq a, Num a) => a -> a -> a" Nothing (Just [ParameterInformation (InR (32,33)) Nothing, ParameterInformation (InR (37,38)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Bool -> Bool -> Bool" Nothing (Just [ParameterInformation (InR (5,9)) Nothing, ParameterInformation (InR (13,17)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "dynamic function"
                  [trimming|
                      f :: Int -> Int -> Int
                      f = _
                      g :: Int -> Int -> Int
                      g = _
                      x = (if _ then f else g) 1 2
                          ^^ ^^^ ^  ^^^ ^  ^^^^^^^^
                  |]
                  (replicate 18 Nothing),
              mkTest
                  "very long type"
                  [trimming|
                      f :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
                      f = _
                      x = f 1
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (12,15)) Nothing, ParameterInformation (InR (19,22)) Nothing, ParameterInformation (InR (26,29)) Nothing, ParameterInformation (InR (33,36)) Nothing, ParameterInformation (InR (40,43)) Nothing, ParameterInformation (InR (47,50)) Nothing, ParameterInformation (InR (54,57)) Nothing, ParameterInformation (InR (61,64)) Nothing, ParameterInformation (InR (68,71)) Nothing, ParameterInformation (InR (75,78)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "very long type with type constraint"
                  [trimming|
                      f :: Num abcdefghijklmn => abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn
                      f = _
                      x = f 1
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall abcdefghijklmn. Num abcdefghijklmn => abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn" Nothing (Just [ParameterInformation (InR (50,64)) Nothing, ParameterInformation (InR (68,82)) Nothing, ParameterInformation (InR (86,100)) Nothing, ParameterInformation (InR (104,118)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Integer -> Integer -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (16,23)) Nothing, ParameterInformation (InR (27,34)) Nothing, ParameterInformation (InR (38,45)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "middle =>"
                  [trimming|
                      f :: Eq a => a -> Num b => b -> b
                      f = _
                      x = f 1 True
                          ^ ^ ^
                      y = f True
                            ^
                      z = f 1
                            ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25,26)) Nothing, ParameterInformation (InR (39,40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Num Bool => Bool -> Bool" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (28,32)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25,26)) Nothing, ParameterInformation (InR (39,40)) Nothing]) (Just (InL 1)), SignatureInformation "f :: Integer -> Num Bool => Bool -> Bool" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (28,32)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25,26)) Nothing, ParameterInformation (InR (39,40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Bool -> Num Integer => Integer -> Integer" Nothing (Just [ParameterInformation (InR (5,9)) Nothing, ParameterInformation (InR (28,35)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25,26)) Nothing, ParameterInformation (InR (39,40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Num Integer => Integer -> Integer" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (31,38)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "=> in argument"
                  [trimming|
                      f :: Eq a => a -> (Num b => b -> b) -> a
                      f = _
                      x = f 1
                          ^ ^
                      y = f 1 negate
                            ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> (Num b => b -> b) -> a" Nothing (Just [ParameterInformation (InR (25,26)) Nothing, ParameterInformation (InR (31,46)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> (Num b => b -> b) -> Integer" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (17,32)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> (Num b => b -> b) -> a" Nothing (Just [ParameterInformation (InR (25,26)) Nothing, ParameterInformation (InR (31,46)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> (Num Any => Any -> Any) -> Integer" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (17,38)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> (Num b => b -> b) -> a" Nothing (Just [ParameterInformation (InR (25,26)) Nothing, ParameterInformation (InR (31,46)) Nothing]) (Just (InL 1)), SignatureInformation "f :: Integer -> (Num Any => Any -> Any) -> Integer" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (17,38)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
                  ],
              mkTest
                  "RankNTypes(forall in middle)"
                  [trimming|
                      f :: Maybe a -> forall b. (a, b) -> b
                      f = _
                      x1 = f Nothing
                           ^ ^
                      x2 = f (Just True)
                             ^
                      x3 = f Nothing (1, True)
                             ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. Maybe a -> forall b. (a, b) -> b" Nothing (Just [ParameterInformation (InR (15,22)) Nothing, ParameterInformation (InR (36,42)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Maybe a -> forall b. (a, b) -> b" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (26,32)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. Maybe a -> forall b. (a, b) -> b" Nothing (Just [ParameterInformation (InR (15,22)) Nothing, ParameterInformation (InR (36,42)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Maybe Bool -> forall b. (Bool, b) -> b" Nothing (Just [ParameterInformation (InR (5,15)) Nothing, ParameterInformation (InR (29,38)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. Maybe a -> forall b. (a, b) -> b" Nothing (Just [ParameterInformation (InR (15,22)) Nothing, ParameterInformation (InR (36,42)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Maybe Integer -> forall b. (Integer, b) -> b" Nothing (Just [ParameterInformation (InR (5,18)) Nothing, ParameterInformation (InR (32,44)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "RankNTypes(forall in middle), another"
                  [trimming|
                      f :: l -> forall a. a -> a
                      f = _
                      x = f 1
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall l. l -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (15,16)) Nothing, ParameterInformation (InR (30,31)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (26,27)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "RankNTypes(forall in middle), again"
                  [trimming|
                      f :: a -> forall a. a -> a
                      f = _
                      x = f 1
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. a -> forall a1. a1 -> a1" Nothing (Just [ParameterInformation (InR (15,16)) Nothing, ParameterInformation (InR (31,33)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (26,27)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "LinearTypes"
                  [trimming|
                      {-# LANGUAGE LinearTypes #-}
                      f :: (a -> b) %1 -> a -> b
                      f = _
                      x1 = f negate
                           ^ ^
                      x2 = f _ 1
                             ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. (a -> b) %1 -> a -> b" Nothing (Just [ParameterInformation (InR (18,24)) Nothing, ParameterInformation (InR (32,33)) Nothing]) (Just (InL 0)), SignatureInformation "f :: (Integer -> Integer) %1 -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (6,24)) Nothing, ParameterInformation (InR (32,39)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b. (a -> b) %1 -> a -> b" Nothing (Just [ParameterInformation (InR (18,24)) Nothing, ParameterInformation (InR (32,33)) Nothing]) (Just (InL 0)), SignatureInformation "f :: (Integer -> b) %1 -> Integer -> b" Nothing (Just [ParameterInformation (InR (6,18)) Nothing, ParameterInformation (InR (26,33)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "function documentation"
                  [trimming|
                      -- |The 'f' function does something to a bool value.
                      f :: Bool -> Bool
                      f = _
                      x = f True
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Bool -> Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nThe  `f`  function does something to a bool value.\n\n") (Just [ParameterInformation (InR (5,9)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "function and arguments documentation"
                  [trimming|
                      -- |Doc for function 'f'.
                      f ::
                        -- | The first 'Bool' argument
                        Bool ->
                        -- | The second 'Int' argument
                        Int ->
                        -- | The return value
                        Bool
                      f = _
                      x = f True 1
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Bool -> Int -> Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nDoc for function  `f` .\n\n") (Just [ParameterInformation (InR (5,9)) (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nThe first  `Bool`  argument\n\n"), ParameterInformation (InR (13,16)) (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nThe second  `Int`  argument\n\n")]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "imported function with no documentation"
                  [trimming|
                      x = even 1
                          ^    ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "even :: forall a. Integral a => a -> Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "\\[Documentation\\]\\(file://.*\\)\n\n\\[Source\\]\\(file://.*\\)\n\n") (Just [ParameterInformation (InR (32,33)) Nothing]) (Just (InL 0)), SignatureInformation "even :: Integer -> Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "\\[Documentation\\]\\(file://.*\\)\n\n\\[Source\\]\\(file://.*\\)\n\n") (Just [ParameterInformation (InR (8,15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "imported function with argument documentation"
                  [trimming|
                      import Language.Haskell.TH.Lib (mkBytes)
                      x = mkBytes _
                          ^       ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "mkBytes :: ForeignPtr Word8 -> Word -> Word -> Bytes" (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nCreate a Bytes datatype representing raw bytes to be embedded into the\n program/library binary.\n\n\\[Documentation\\]\\(file://.*\\)\n\n\\[Source\\]\\(file://.*\\)\n\n") (Just [ParameterInformation (InR (11,27)) (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nPointer to the data\n\n"), ParameterInformation (InR (31,35)) (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nOffset from the pointer\n\n"), ParameterInformation (InR (39,43)) (Just $ InR $ MarkupContent MarkupKind_Markdown "\n\nNumber of bytes\n\n")]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "TypeApplications"
                  [trimming|
                      f :: a -> b -> c
                      f = _
                      x = f @Int @_ 1 True
                          ^  ^    ^ ^
                  |]
                  [ Nothing,
                    Nothing,
                    Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a b c. a -> b -> c" Nothing (Just [ParameterInformation (InR (19,20)) Nothing, ParameterInformation (InR (24,25)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ]
            ]

mkTest :: TestName -> Text -> [Maybe SignatureHelp] -> TestTree
mkTest name sourceCode expectedSignatureHelps =
    parameterisedCursorTest
        name
        sourceCode
        expectedSignatureHelps
        getSignatureHelpFromSession

getSignatureHelpFromSession :: Text -> PosPrefixInfo -> IO (Maybe SignatureHelp)
getSignatureHelpFromSession sourceCode (PosPrefixInfo _ _ _ position) =
    let fileName = "A.hs"
        plugin = mkPluginTestDescriptor descriptor "signatureHelp"
        virtualFileTree = mkVirtualFileTreeWithSingleFile fileName sourceCode
     in runSessionWithServerInTmpDir def plugin virtualFileTree $ do
            doc <- openDoc fileName "haskell"
            (fmap . fmap) mkReproducibleSignatureHelp (getSignatureHelp doc position)

mkReproducibleSignatureHelp :: SignatureHelp -> SignatureHelp
mkReproducibleSignatureHelp = L.signatures . traverse . L.documentation %~ unifyLocalFilePath
    where
        unifyLocalFilePath (Just (InR (MarkupContent MarkupKind_Markdown doc))) =
            let (prefix, match, suffix) = doc =~ documentationRegex :: (Text, Text, Text)
                (prefix', match', suffix') = suffix =~ sourceRegex :: (Text, Text, Text)
                reproducibleDoc =
                    if T.null match
                        then prefix
                        else
                            prefix
                                <> documentationRegex
                                <> ( if T.null match'
                                         then prefix'
                                         else prefix' <> sourceRegex <> suffix'
                                   )
             in Just $ InR $ MarkupContent MarkupKind_Markdown reproducibleDoc
        unifyLocalFilePath mDoc = mDoc
        documentationRegex = "\\[Documentation\\]\\(file://.*\\)\n\n"
        sourceRegex = "\\[Source\\]\\(file://.*\\)\n\n"

mkVirtualFileTreeWithSingleFile :: FilePath -> Text -> VirtualFileTree
mkVirtualFileTreeWithSingleFile fileName sourceCode =
    let testDataDir = "/not-used-dir"
     in mkVirtualFileTree
            testDataDir
            [ directCradle [T.pack fileName],
              file fileName (text sourceCode)
            ]

-- TODO(@linj) use the one from lsp-test when we have https://github.com/haskell/lsp/pull/621
-- | Returns the signature help at the specified position.
getSignatureHelp :: TextDocumentIdentifier -> Position -> Session (Maybe SignatureHelp)
getSignatureHelp doc pos =
    let params = SignatureHelpParams doc pos Nothing Nothing
     in nullToMaybe . getResponseResult <$> request SMethod_TextDocumentSignatureHelp params
    where
        getResponseResult rsp =
            case rsp ^. L.result of
                Right x -> x
                Left err -> throw $ UnexpectedResponseError (fromJust $ rsp ^. L.id) err
