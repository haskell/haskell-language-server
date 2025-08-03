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
                  "type constraint"
                  [trimming|
                      f :: (Num a) => a -> a -> a
                      f = _
                      x = f 1 2
                          ^ ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. Num a => a -> a -> a" Nothing (Just [ParameterInformation (InR (24,25)) Nothing, ParameterInformation (InR (29,30)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0)),
                    Just $ SignatureHelp [SignatureInformation "f :: forall a. Num a => a -> a -> a" Nothing (Just [ParameterInformation (InR (24,25)) Nothing, ParameterInformation (InR (29,30)) Nothing]) (Just (InL 1))] (Just 0) (Just (InL 1))
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
                  "multi-line type"
                  [trimming|
                      f :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
                      f = _
                      x = f 1
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: Int\n-> Int\n-> Int\n-> Int\n-> Int\n-> Int\n-> Int\n-> Int\n-> Int\n-> Int\n-> Int\n-> Int" Nothing (Just [ParameterInformation (InR (5,8)) Nothing, ParameterInformation (InR (13,16)) Nothing, ParameterInformation (InR (21,24)) Nothing, ParameterInformation (InR (29,32)) Nothing, ParameterInformation (InR (37,40)) Nothing, ParameterInformation (InR (45,48)) Nothing, ParameterInformation (InR (53,56)) Nothing, ParameterInformation (InR (61,64)) Nothing, ParameterInformation (InR (69,72)) Nothing, ParameterInformation (InR (77,80)) Nothing, ParameterInformation (InR (85,88)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
                  ],
              mkTest
                  "multi-line type with type constraint"
                  [trimming|
                      f :: Num abcdefghijklmn => abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn -> abcdefghijklmn
                      f = _
                      x = f 1
                          ^ ^
                  |]
                  [ Nothing,
                    Just $ SignatureHelp [SignatureInformation "f :: forall abcdefghijklmn.\nNum abcdefghijklmn =>\nabcdefghijklmn\n-> abcdefghijklmn\n-> abcdefghijklmn\n-> abcdefghijklmn\n-> abcdefghijklmn" Nothing (Just [ParameterInformation (InR (52,66)) Nothing, ParameterInformation (InR (71,85)) Nothing, ParameterInformation (InR (90,104)) Nothing, ParameterInformation (InR (109,123)) Nothing]) (Just (InL 0)), SignatureInformation "f :: Integer -> Integer -> Integer -> Integer -> Integer" Nothing (Just [ParameterInformation (InR (5,12)) Nothing, ParameterInformation (InR (16,23)) Nothing, ParameterInformation (InR (27,34)) Nothing, ParameterInformation (InR (38,45)) Nothing]) (Just (InL 0))] (Just 0) (Just (InL 0))
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
