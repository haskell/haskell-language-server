module Development.IDE.Session.Diagnostics where
import           Control.Monad
import qualified Data.Aeson                        as Aeson
import           Data.Char                         (isLower)
import           Data.List
import           Data.List.Extra                   (dropPrefix, split)
import           Data.Maybe
import qualified Data.Text                         as T
import qualified Data.Vector                       as Vector
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import qualified HIE.Bios.Cradle                   as HieBios
import           HIE.Bios.Types                    hiding (Log)
import           System.FilePath

{- | Takes a cradle error, the corresponding cradle and the file path where
  the cradle error occurred (of the file we attempted to load).
  Depicts the cradle error in a user-friendly way.
-}
renderCradleError :: CradleError -> Cradle a -> NormalizedFilePath -> FileDiagnostic
renderCradleError (CradleError deps _ec ms) cradle nfp
  | HieBios.isCabalCradle cradle && any (isInfixOf "Error: cabal: Failed extracting script block:") ms =
      let (fp, showDiag, diag) = ideErrorWithSource (Just "cradle") (Just DiagnosticSeverity_Error) nfp $ T.unlines $ map T.pack userFriendlyMessage in
        (fp, showDiag, diag{_data_ = Just (Aeson.Array $ Vector.fromList $ map (Aeson.String . T.pack) absDeps)})
  | otherwise = ideErrorWithSource (Just "cradle") (Just DiagnosticSeverity_Error) nfp $ T.unlines $ map T.pack userFriendlyMessage
  where
    absDeps = fmap (cradleRootDir cradle </>) deps
    userFriendlyMessage :: [String]
    userFriendlyMessage
      | HieBios.isCabalCradle cradle = fromMaybe ms fileMissingMessage
      | otherwise = ms

    fileMissingMessage :: Maybe [String]
    fileMissingMessage =
      multiCradleErrMessage <$> parseMultiCradleErr ms

-- | Information included in Multi Cradle error messages
data MultiCradleErr = MultiCradleErr
  { mcPwd      :: FilePath
  , mcFilePath :: FilePath
  , mcPrefixes :: [(FilePath, String)]
  } deriving (Show)

-- | Attempt to parse a multi-cradle message
parseMultiCradleErr :: [String] -> Maybe MultiCradleErr
parseMultiCradleErr ms = do
  _  <- lineAfter "Multi Cradle: "
  wd <- lineAfter "pwd: "
  fp <- lineAfter "filepath: "
  ps <- prefixes
  pure $ MultiCradleErr wd fp ps

  where
    lineAfter :: String -> Maybe String
    lineAfter pre = listToMaybe $ mapMaybe (stripPrefix pre) ms

    prefixes :: Maybe [(FilePath, String)]
    prefixes = do
      pure $ mapMaybe tuple ms

    tuple :: String -> Maybe (String, String)
    tuple line = do
      line' <- surround '(' line ')'
      [f, s] <- pure $ split (==',') line'
      pure (f, s)

    -- extracts the string surrounded by required characters
    surround :: Char -> String -> Char -> Maybe String
    surround start s end = do
      guard (listToMaybe s == Just start)
      guard (listToMaybe (reverse s) == Just end)
      pure $ drop 1 $ take (length s - 1) s

multiCradleErrMessage :: MultiCradleErr -> [String]
multiCradleErrMessage e =
    [ "Loading the module '" <> moduleFileName <> "' failed. It may not be listed in your .cabal file!"
    , "Perhaps you need to add `"<> moduleName <> "` to other-modules or exposed-modules."
    , "For more information, visit: https://cabal.readthedocs.io/en/3.4/developing-packages.html#modules-included-in-the-package"
    , ""
    ] <> map prefix (mcPrefixes e)
  where
    localFilePath f = dropWhile (==pathSeparator) $ dropPrefix (mcPwd e) f
    moduleFileName = localFilePath $ mcFilePath e
    moduleName = intercalate "." $ map dropExtension $ dropWhile isSourceFolder $ splitDirectories moduleFileName
    isSourceFolder p = all isLower $ take 1 p
    prefix (f, r) = f <> " - " <> r
