{-# LANGUAGE QuasiQuotes #-}

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
                    Just $ SignatureHelp [SignatureInformation "pure :: forall (f :: Type -> Type) a. Applicative f => a -> f a" Nothing (Just [ParameterInformation (InR (55,56)) Nothing]) (Just (InL 0)), SignatureInformation "pure :: Bool -> IO Bool" Nothing (Just [ParameterInformation (InR (8,12)) Nothing]) (Just (InL 0)), SignatureInformation "pure :: forall a. a -> IO a" Nothing (Just [ParameterInformation (InR (18,19)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
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
            getSignatureHelp doc position

mkVirtualFileTreeWithSingleFile :: FilePath -> Text -> VirtualFileTree
mkVirtualFileTreeWithSingleFile fileName sourceCode =
    let testDataDir = "/not-used-dir"
     in mkVirtualFileTree
            testDataDir
            [ directCradle [T.pack fileName],
              file fileName (text sourceCode)
            ]

-- TODO(@linj) upstream it to lsp-test
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
