{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

import           Control.Arrow                            ((>>>))
import           Control.Exception                        (throw)
import           Control.Lens                             ((^.))
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

main :: IO ()
main =
  defaultTestRunner $
    testGroup
      "signatureHelp"
      [ mkTest
          "1 parameter"
          [__i|
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
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "2 parameters"
          [__i|
            f :: Int -> Int -> Int
            f = _
            x = f 1 2
                ^ ^^^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
          ],
        mkTest
          "3 parameters"
          [__i|
            f :: Int -> Int -> Int -> Int
            f = _
            x = f 1 2 3
                ^ ^ ^ ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing, ParameterInformation (InR (19, 22)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing, ParameterInformation (InR (19, 22)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing, ParameterInformation (InR (19, 22)) Nothing]) (Just (InL 2))] (Just 0) (Just (InL 2))
          ],
        mkTest
          "parentheses"
          [__i|
            f :: Int -> Int -> Int
            f = _
            x = (f 1) 2
                ^^ ^^^^
          |]
          [ Nothing,
            Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
          ],
        mkTest
          "newline"
          [__i|
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
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1)),
            Nothing
          ],
        mkTest
          "nested"
          [__i|
            f :: Int -> Int -> Int
            f = _
            g :: Int -> Int
            g = _
            x = f (g 1) 2
                ^^^^ ^^^^
          |]
          [ Nothing,
            Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "g :: Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "g :: Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
          ],
        mkTest
          "higher-order function"
          [__i|
            f :: (Int -> Int) -> Int -> Int
            f = _
            x = f (+ 1) 2
                ^ ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: (Int -> Int) -> Int -> Int" Nothing (Just [ParameterInformation (InR (6, 16)) Nothing, ParameterInformation (InR (21, 24)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "type constraint"
          [__i|
            f :: (Num a) => a -> a -> a
            f = _
            x = f 1 2
                ^ ^ ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a. Num a => a -> a -> a" Nothing (Just [ParameterInformation (InR (24, 25)) Nothing, ParameterInformation (InR (29, 30)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (16, 23)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a. Num a => a -> a -> a" Nothing (Just [ParameterInformation (InR (24, 25)) Nothing, ParameterInformation (InR (29, 30)) Nothing]) (Just (InL 1)), SignatureInformation "f :: Integer -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (16, 23)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
          ],
        mkTest
          "type constraint with kind signatures"
          [__i|
            x :: IO Bool
            x = pure True
                 ^   ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "pure :: forall (f :: Type -> Type) a. Applicative f => a -> f a" (Just $ InR $ MarkupContent MarkupKind_Markdown "Lift a value") (Just [ParameterInformation (InR (55, 56)) Nothing]) (Just (InL 0)), SignatureInformation "pure :: Bool -> IO Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "Lift a value") (Just [ParameterInformation (InR (8, 12)) Nothing]) (Just (InL 0)), SignatureInformation "pure :: forall a. a -> IO a" (Just $ InR $ MarkupContent MarkupKind_Markdown "Lift a value") (Just [ParameterInformation (InR (18, 19)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "2 type constraints"
          [__i|
            f :: forall a. (Eq a, Num a) => a -> a -> a
            f = _
            x = f True
                ^   ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a. (Eq a, Num a) => a -> a -> a" Nothing (Just [ParameterInformation (InR (32, 33)) Nothing, ParameterInformation (InR (37, 38)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Bool -> Bool -> Bool" Nothing (Just [ParameterInformation (InR (5, 9)) Nothing, ParameterInformation (InR (13, 17)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "dynamic function"
          [__i|
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
          [__i|
            f :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
            f = _
            x = f 1
                ^ ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int" Nothing (Just [ParameterInformation (InR (5, 8)) Nothing, ParameterInformation (InR (12, 15)) Nothing, ParameterInformation (InR (19, 22)) Nothing, ParameterInformation (InR (26, 29)) Nothing, ParameterInformation (InR (33, 36)) Nothing, ParameterInformation (InR (40, 43)) Nothing, ParameterInformation (InR (47, 50)) Nothing, ParameterInformation (InR (54, 57)) Nothing, ParameterInformation (InR (61, 64)) Nothing, ParameterInformation (InR (68, 71)) Nothing, ParameterInformation (InR (75, 78)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "very long type with type constraint"
          [__i|
            f :: Num abcdefghijklmn => abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn
            f = _
            x = f 1
                ^ ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall abcdefghijklmn. Num abcdefghijklmn => abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn" Nothing (Just [ParameterInformation (InR (50, 64)) Nothing, ParameterInformation (InR (68, 82)) Nothing, ParameterInformation (InR (86, 100)) Nothing, ParameterInformation (InR (104, 118)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Integer -> Integer -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (16, 23)) Nothing, ParameterInformation (InR (27, 34)) Nothing, ParameterInformation (InR (38, 45)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        -- TODO fix bug of wrong parameter range in the function type string
        -- https://github.com/haskell/haskell-language-server/pull/4626#discussion_r2261133076
        mkTestExpectFail
          "middle =>"
          [__i|
            f :: Eq a => a -> Num b => b -> b
            f = _
            x = f 1 True
                ^ ^ ^
            y = f True
                  ^
            z = f 1
                  ^
          |]
          ( BrokenIdeal
              [ Nothing,
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (39, 40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Num Bool => Bool -> Bool" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (28, 32)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (39, 40)) Nothing]) (Just (InL 1)), SignatureInformation "f :: Integer -> Num Bool => Bool -> Bool" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (28, 32)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1)),
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (39, 40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Bool -> Num Integer => Integer -> Integer" Nothing (Just [ParameterInformation (InR (5, 9)) Nothing, ParameterInformation (InR (28, 35)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (39, 40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Num Integer => Integer -> Integer" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (31, 38)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
              ]
          )
          ( BrokenCurrent
              [ Nothing,
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (39, 40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Num Bool => Bool -> Bool" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (28, 32)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (39, 40)) Nothing]) (Just (InL 1)), SignatureInformation "f :: Integer -> Num Bool => Bool -> Bool" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (28, 32)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1)),
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (39, 40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Bool -> Num Integer => Integer -> Integer" Nothing (Just [ParameterInformation (InR (5, 9)) Nothing, ParameterInformation (InR (28, 35)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> Num b => b -> b" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (39, 40)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Num Integer => Integer -> Integer" Nothing (Just [ParameterInformation (InR (20, 27)) Nothing, ParameterInformation (InR (31, 38)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
              ]
          ),
        mkTest
          "=> in parameter"
          [__i|
            f :: Eq a => a -> (Num b => b -> b) -> a
            f = _
            x = f 1
                ^ ^
            y = f 1 negate
                  ^ ^
          |]
          ( let typ =
                  if ghcVersion <= GHC98
                    then "f :: Integer -> (Num Any => Any -> Any) -> Integer"
                    else "f :: Integer -> (Num (ZonkAny 0) => ZonkAny 0 -> ZonkAny 0) -> Integer"
                range = if ghcVersion <= GHC98 then (17, 38) else (17, 58)
             in [ Nothing,
                  Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> (Num b => b -> b) -> a" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (31, 46)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> (Num b => b -> b) -> Integer" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (17, 32)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                  Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> (Num b => b -> b) -> a" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (31, 46)) Nothing]) (Just (InL 0)), SignatureInformation typ Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR range) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                  Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. Eq a => a -> (Num b => b -> b) -> a" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (31, 46)) Nothing]) (Just (InL 1)), SignatureInformation typ Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR range) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
                ]
          ),
        mkTest
          "RankNTypes(forall in middle)"
          [__i|
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
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a. Maybe a -> forall b. (a, b) -> b" Nothing (Just [ParameterInformation (InR (15, 22)) Nothing, ParameterInformation (InR (36, 42)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Maybe a -> forall b. (a, b) -> b" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (26, 32)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a. Maybe a -> forall b. (a, b) -> b" Nothing (Just [ParameterInformation (InR (15, 22)) Nothing, ParameterInformation (InR (36, 42)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Maybe Bool -> forall b. (Bool, b) -> b" Nothing (Just [ParameterInformation (InR (5, 15)) Nothing, ParameterInformation (InR (29, 38)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a. Maybe a -> forall b. (a, b) -> b" Nothing (Just [ParameterInformation (InR (15, 22)) Nothing, ParameterInformation (InR (36, 42)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Maybe Integer -> forall b. (Integer, b) -> b" Nothing (Just [ParameterInformation (InR (5, 18)) Nothing, ParameterInformation (InR (32, 44)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        -- TODO fix bug of wrong parameter range in the function type string
        -- https://github.com/haskell/haskell-language-server/pull/4626#discussion_r2261133076
        mkTestExpectFail
          "RankNTypes(forall in middle), another"
          [__i|
            f :: l -> forall a. a -> a
            f = _
            x = f 1
                ^ ^
          |]
          ( BrokenIdeal
              [ Nothing,
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall l. l -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (15, 16)) Nothing, ParameterInformation (InR (30, 31)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (26, 27)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
              ]
          )
          ( BrokenCurrent
              [ Nothing,
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall l. l -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (25, 26)) Nothing, ParameterInformation (InR (30, 31)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (26, 27)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
              ]
          ),
        -- TODO fix bug of wrong parameter range in the function type string
        -- https://github.com/haskell/haskell-language-server/pull/4626#discussion_r2261133076
        mkTestExpectFail
          "RankNTypes(forall in middle), again"
          [__i|
            f :: a -> forall a. a -> a
            f = _
            x = f 1
                ^ ^
          |]
          ( BrokenIdeal
              [ Nothing,
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a. a -> forall a1. a1 -> a1" Nothing (Just [ParameterInformation (InR (15, 16)) Nothing, ParameterInformation (InR (31, 33)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (26, 27)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
              ]
          )
          ( BrokenCurrent
              [ Nothing,
                Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a. a -> forall a1. a1 -> a1" Nothing (Just [ParameterInformation (InR (27, 28)) Nothing, ParameterInformation (InR (31, 32)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> forall a. a -> a" Nothing (Just [ParameterInformation (InR (5, 12)) Nothing, ParameterInformation (InR (26, 27)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
              ]
          ),
        mkTest
          "LinearTypes"
          [__i|
            {-\# LANGUAGE LinearTypes \#-}
            f :: (a -> b) %1 -> a -> b
            f = _
            x1 = f negate
                 ^ ^
            x2 = f _ 1
                   ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. (a -> b) %1 -> a -> b" Nothing (Just [ParameterInformation (InR (18, 24)) Nothing, ParameterInformation (InR (32, 33)) Nothing]) (Just (InL 0)), SignatureInformation "f :: (Integer -> Integer) %1 -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (6, 24)) Nothing, ParameterInformation (InR (32, 39)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b. (a -> b) %1 -> a -> b" Nothing (Just [ParameterInformation (InR (18, 24)) Nothing, ParameterInformation (InR (32, 33)) Nothing]) (Just (InL 0)), SignatureInformation "f :: (Integer -> b) %1 -> Integer -> b" Nothing (Just [ParameterInformation (InR (6, 18)) Nothing, ParameterInformation (InR (26, 33)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "function documentation"
          [__i|
            -- |The 'f' function does something to a bool value.
            f :: Bool -> Bool
            f = _
            x = f True
                ^ ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Bool -> Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "The  `f`  function does something to a bool value") (Just [ParameterInformation (InR (5, 9)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "function and parameters documentation"
          [__i|
            -- |Doc for function 'f'.
            f ::
              -- | The first 'Bool' parameter
              Bool ->
              -- | The second 'Int' parameter
              Int ->
              -- | The return value
              Bool
            f = _
            x = f True 1
                ^ ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: Bool -> Int -> Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "Doc for function  `f`") (Just [ParameterInformation (InR (5, 9)) (Just $ InR $ MarkupContent MarkupKind_Markdown "The first  `Bool`  parameter"), ParameterInformation (InR (13, 16)) (Just $ InR $ MarkupContent MarkupKind_Markdown "The second  `Int`  parameter")]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "imported function with no documentation"
          [__i|
            x = even 1
                ^    ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "even :: forall a. Integral a => a -> Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "") (Just [ParameterInformation (InR (32, 33)) Nothing]) (Just (InL 0)), SignatureInformation "even :: Integer -> Bool" (Just $ InR $ MarkupContent MarkupKind_Markdown "") (Just [ParameterInformation (InR (8, 15)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "imported function with parameter documentation"
          [__i|
            import Language.Haskell.TH.Lib (mkBytes)
            x = mkBytes _
                ^       ^
          |]
          [ Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "mkBytes :: ForeignPtr Word8 -> Word -> Word -> Bytes" (Just $ InR $ MarkupContent MarkupKind_Markdown "Create a Bytes datatype representing raw bytes to be embedded into the") (Just [ParameterInformation (InR (11, 27)) (Just $ InR $ MarkupContent MarkupKind_Markdown "Pointer to the data"), ParameterInformation (InR (31, 35)) (Just $ InR $ MarkupContent MarkupKind_Markdown "Offset from the pointer"), ParameterInformation (InR (39, 43)) (Just $ InR $ MarkupContent MarkupKind_Markdown "Number of bytes")]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ],
        mkTest
          "TypeApplications"
          [__i|
            f :: a -> b -> c
            f = _
            x = f @Int @_ 1 True
                ^  ^    ^ ^
          |]
          [ Nothing,
            Nothing,
            Nothing,
            Just $ SimilarSignatureHelp $ SignatureHelp [SignatureInformation "f :: forall a b c. a -> b -> c" Nothing (Just [ParameterInformation (InR (19, 20)) Nothing, ParameterInformation (InR (24, 25)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
          ]
      ]

mkTest :: TestName -> Text -> [Maybe SimilarSignatureHelp] -> TestTree
mkTest name sourceCode expectedSignatureHelps =
  parameterisedCursorTest
    name
    sourceCode
    expectedSignatureHelps
    getSignatureHelpFromSession

mkTestExpectFail ::
  TestName ->
  Text ->
  ExpectBroken 'Ideal [Maybe SimilarSignatureHelp] ->
  ExpectBroken 'Current [Maybe SimilarSignatureHelp] ->
  TestTree
mkTestExpectFail name sourceCode _idealSignatureHelps = unCurrent >>> mkTest name sourceCode

getSignatureHelpFromSession :: Text -> PosPrefixInfo -> IO (Maybe SimilarSignatureHelp)
getSignatureHelpFromSession sourceCode (PosPrefixInfo _ _ _ position) =
  let fileName = "A.hs"
      plugin = mkPluginTestDescriptor descriptor "signatureHelp"
      virtualFileTree = mkVirtualFileTreeWithSingleFile fileName sourceCode
   in runSessionWithServerInTmpDir def plugin virtualFileTree $ do
        doc <- openDoc fileName "haskell"
        (fmap . fmap) SimilarSignatureHelp (getSignatureHelp doc position)

mkVirtualFileTreeWithSingleFile :: FilePath -> Text -> VirtualFileTree
mkVirtualFileTreeWithSingleFile fileName sourceCode =
  let testDataDir = "/not-used-dir"
   in mkVirtualFileTree
        testDataDir
        [ directCradle [T.pack fileName],
          file fileName (text sourceCode)
        ]

newtype SimilarSignatureHelp = SimilarSignatureHelp SignatureHelp
  deriving newtype (Show)

-- custom Eq to ignore some details, such as added doc string
-- not symmetry
instance Eq SimilarSignatureHelp where
  SimilarSignatureHelp
    actualSignatureHelp@( SignatureHelp
                            actualSignatureInformations
                            actualActiveSignature
                            actualActiveParameter
                          )
    == SimilarSignatureHelp
         expectedSignatureHelp@( SignatureHelp
                                   expectedSignatureInformations
                                   expectedActiveSignature
                                   expectedActiveParameter
                                 )
      | actualSignatureHelp == expectedSignatureHelp = True
      | actualActiveSignature == expectedActiveSignature
          && actualActiveParameter == expectedActiveParameter =
          actualSignatureInformations ~= expectedSignatureInformations
      | otherwise = False

class IsSimilar a where
  (~=) :: a -> a -> Bool

instance IsSimilar SignatureInformation where
  actualSignatureInformation@( SignatureInformation
                                 actualLabel
                                 actualDocumentation
                                 actualParameters
                                 actualActiveParameter
                               )
    ~= expectedSignatureInformation@( SignatureInformation
                                        expectedLabel
                                        expectedDocumentation
                                        expectedParameters
                                        expectedActiveParameter
                                      )
      | actualSignatureInformation == expectedSignatureInformation = True
      | actualLabel == expectedLabel && actualActiveParameter == expectedActiveParameter =
          actualDocumentation ~= expectedDocumentation
            && actualParameters ~= expectedParameters
      | otherwise = False

instance IsSimilar ParameterInformation where
  actualParameterInformation@(ParameterInformation actualLabel actualDocumentation)
    ~= expectedParameterInformation@(ParameterInformation expectedLabel expectedDocumentation)
      | actualParameterInformation == expectedParameterInformation = True
      | actualLabel == expectedLabel = actualDocumentation ~= expectedDocumentation
      | otherwise = False

instance IsSimilar MarkupContent where
  actualMarkupContent@(MarkupContent actualKind actualText)
    ~= expectedMarkupContent@(MarkupContent expectedKind expectedText)
      | actualMarkupContent == expectedMarkupContent = True
      | actualKind == expectedKind = actualText ~= expectedText
      | otherwise = False

instance IsSimilar Text where
  actualText ~= expectedText = expectedText `T.isInfixOf` actualText

instance (IsSimilar a) => IsSimilar [a] where
  [] ~= []             = True
  (x : xs) ~= (y : ys) = x ~= y && xs ~= ys
  _ ~= _               = False

instance (IsSimilar a) => IsSimilar (Maybe a) where
  Nothing ~= Nothing = True
  Just x ~= Just y   = x ~= y
  _ ~= _             = False

instance (IsSimilar a, IsSimilar b) => IsSimilar (a |? b) where
  InL x ~= InL y = x ~= y
  InR x ~= InR y = x ~= y
  _ ~= _         = False

-- TODO use the one from lsp-test when we have https://github.com/haskell/lsp/pull/621

-- | Returns the signature help at the specified position.
getSignatureHelp :: TextDocumentIdentifier -> Position -> Session (Maybe SignatureHelp)
getSignatureHelp doc pos =
  let params = SignatureHelpParams doc pos Nothing Nothing
   in nullToMaybe . getResponseResult <$> request SMethod_TextDocumentSignatureHelp params
  where
    getResponseResult rsp =
      case rsp ^. L.result of
        Right x  -> x
        Left err -> throw $ UnexpectedResponseError (fromJust $ rsp ^. L.id) err
