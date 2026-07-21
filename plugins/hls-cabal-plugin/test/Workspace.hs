{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Workspace (

) where

import Control.Lens ((^.), (^?))
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Distribution.PackageDescription (PackageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Pretty qualified as Pretty
import Distribution.Types.ComponentName (ComponentName)
import Ide.Plugin.Cabal.Parse (parseCabalFileContents)
import Language.LSP.Protocol.Lens qualified as L
import Test.Hls

cabalWorkspaceTests :: TestTree
cabalWorkspaceTests =
  testGroup
    "Workspace"
    []
 where
  generateWorkspaceFileRenameTestSession :: FilePath -> FilePath -> ComponentName -> Session PackageDescription
  generateWorkspaceFileRenameTestSession cabalFile haskellFile compName = do
    haskellDoc <- openDoc haskellFile "haskell"
    cabalDoc <- openDoc cabalFile "cabal"
    _ <- waitForDiagnosticsFrom haskellDoc
    cas <- Maybe.mapMaybe (^? _R) <$> getAllCodeActions haskellDoc
    let selectedCas = filter (\ca -> (T.pack $ "Add to " <> Pretty.prettyShow compName <> " ") `T.isPrefixOf` (ca ^. L.title)) cas
    mapM_ executeCodeAction $ selectedCas
    _ <- skipManyTill anyMessage $ getDocumentEdit cabalDoc -- Wait for the changes in cabal file
    contents <- documentContents cabalDoc
    case parseCabalFileContents $ T.encodeUtf8 contents of
      (_, Right gpd) -> pure $ flattenPackageDescription gpd
      _ -> liftIO $ assertFailure "could not parse cabal file to gpd"
