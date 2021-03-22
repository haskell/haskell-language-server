{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Class
  ( tests
  )
where

import           Control.Lens            hiding ((<.>))
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Text.Encoding      as T
import qualified Language.LSP.Types.Lens as J
import           System.FilePath
import           Test.Hls

tests :: TestTree
tests = testGroup
  "class"
  [ testCase "Produces addMinimalMethodPlaceholders code actions for one instance" $ do
      runSession hlsCommand fullCaps classPath $ do
        doc <- openDoc "T1.hs" "haskell"
        _ <- waitForDiagnosticsFromSource doc "typecheck"
        caResults <- getAllCodeActions doc
        liftIO $ map (^? _CACodeAction . J.title) caResults
          @?=
          [ Just "Add placeholders for '=='"
          , Just "Add placeholders for '/='"
          , Just "Disable \"missing-methods\" warnings"
          ]
  , glodenTest "Creates a placeholder for '=='" "T1" "eq"
    $ \(eqAction:_) -> do
      executeCodeAction eqAction
  , glodenTest "Creates a placeholder for '/='" "T1" "ne"
    $ \(_:neAction:_) -> do
      executeCodeAction neAction
  , glodenTest "Creates a placeholder for 'fmap'" "T2" "fmap"
    $ \(_:_:fmapAction:_) -> do
      executeCodeAction fmapAction
  , glodenTest "Creates a placeholder for multiple methods 1" "T3" "1"
    $ \(mmAction:_) -> do
      executeCodeAction mmAction
  , glodenTest "Creates a placeholder for multiple methods 2" "T3" "2"
    $ \(_:mmAction:_) -> do
      executeCodeAction mmAction
  , glodenTest "Creates a placeholder for a method starting with '_'" "T4" ""
    $ \(_fAction:_) -> do
      executeCodeAction _fAction
  ]

_CACodeAction :: Prism' (Command |? CodeAction) CodeAction
_CACodeAction = prism' InR $ \case
  InR action -> Just action
  _          -> Nothing

classPath :: FilePath
classPath = "test" </> "testdata" </> "class"

glodenTest :: String -> FilePath -> FilePath -> ([CodeAction] -> Session ()) -> TestTree
glodenTest name fp deco execute
  = goldenGitDiff name (classPath </> fpWithDeco <.> "expected" <.> "hs")
    $ runSession hlsCommand fullCaps classPath
    $ do
      doc <- openDoc (fp <.> "hs") "haskell"
      _ <- waitForDiagnosticsFromSource doc "typecheck"
      actions <- concatMap (^.. _CACodeAction) <$> getAllCodeActions doc
      execute actions
      BS.fromStrict . T.encodeUtf8 <$> skipManyTill anyMessage (getDocumentEdit doc)
  where
    fpWithDeco
      | deco == "" = fp
      | otherwise  = fp <.> deco
