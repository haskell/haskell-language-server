{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

import           Control.Exception          (throw)
import           Control.Lens               hiding ((<.>))
import           Control.Monad              (void)
import           Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Text                  as T
import           Ide.Plugin.DocumentLink    (descriptor)
import qualified Ide.Plugin.DocumentLink    as DL
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath
import           Test.Hls
import qualified Test.Hls.FileSystem        as FS

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-document-link-plugin" </> "test" </> "testdata"

main :: IO ()
main = defaultTestRunner tests

tests :: TestTree
tests = testGroup "documentLink"
  [ goldenTest "no document links" "NoDocumentLinks" []
  , goldenTest "links of primitive types" "Definition"
      [ (mkDocLink (mkRange 0 10 0 13) (Uri "GHC-Types.html#t:Int"))
      , (mkDocLink (mkRange 2 10 2 14) (Uri "GHC-Types.html#t:Bool"))
      , (mkDocLink (mkRange 3 9 3 13) (Uri "GHC-Types.html#v:True"))
      ]
  , goldenTest "links from modules" "ImportModule"
      [ (mkDocLink (mkRange 0 19 0 27) (Uri "GHC-Internal-Data-Maybe.html#v:fromJust"))
      , (mkDocLink (mkRange 0 29 0 38) (Uri "GHC-Internal-Data-Maybe.html#v:fromMaybe"))
      , (mkDocLink (mkRange 1 5 1 13) (Uri "GHC-Internal-Data-Maybe.html#v:fromJust"))
      , (mkDocLink (mkRange 1 15 1 24) (Uri "GHC-Internal-Data-Maybe.html#v:fromMaybe"))
      ]
  ]

mkDocLink :: Range -> Uri -> SimilarDocumentLink
mkDocLink range uri =
  SimilarDocumentLink (DocumentLink range (Just uri) Nothing Nothing)

goldenTest :: TestName -> FilePath -> [SimilarDocumentLink] -> TestTree
goldenTest title file expected = testCase title $ runWithDocumentLink filehs $ do
  adoc <- openDoc filehs "haskell"
  void waitForBuildQueue
  documentLink <- getDocumentLink adoc
  liftIO $ ((fmap . fmap) SimilarDocumentLink documentLink) @?= Just expected
  where filehs = file <.> "hs"

runWithDocumentLink :: FilePath -> Session a -> IO a
runWithDocumentLink file = runSessionWithServerInTmpDir def plugin (mkFs $ FS.directProject file)
  where plugin :: PluginTestDescriptor DL.Log
        plugin = mkPluginTestDescriptor descriptor "documentLink"
        mkFs :: [FS.FileTree] -> FS.VirtualFileTree
        mkFs = FS.mkVirtualFileTree testDataDir

getDocumentLink :: TextDocumentIdentifier -> Session (Maybe [DocumentLink])
getDocumentLink doc =
  let params = DocumentLinkParams Nothing Nothing doc
  in nullToMaybe . getResponseResult <$> request SMethod_TextDocumentDocumentLink params


getResponseResult :: (Show (ErrorData m)) => TResponseMessage m -> MessageResult m
getResponseResult rsp =
  case rsp ^. L.result of
    Right x  -> x
    Left err -> throw $ UnexpectedResponseError (fromJust $ rsp ^. L.id) err

newtype SimilarDocumentLink = SimilarDocumentLink DocumentLink
  deriving newtype (Show)

-- custom Eq to ignore some details, such as specific URI
-- not symmetry
instance Eq SimilarDocumentLink where
  SimilarDocumentLink actualDocumentLink@( DocumentLink
                                             actualRange
                                             actualUri
                                             actualTooltip
                                             actualData )
    == SimilarDocumentLink expectedDocumentLink@( DocumentLink
                                                    expectRange
                                                    expectUri
                                                    expectToolTip
                                                    expectData )
    | actualDocumentLink == expectedDocumentLink = True
    | actualRange == expectRange
      && actualTooltip == expectToolTip
      && actualData == expectData
      = actualUri ~= expectUri
    | otherwise = False

class IsSimilar a where
  (~=) :: a -> a -> Bool

instance (IsSimilar a) => IsSimilar (Maybe a) where
  m1 ~= m2 = fromMaybe False $ liftA2 (~=) m1 m2

instance IsSimilar Uri where
  (Uri actual) ~= (Uri except)
    = except `T.isSuffixOf` actual
