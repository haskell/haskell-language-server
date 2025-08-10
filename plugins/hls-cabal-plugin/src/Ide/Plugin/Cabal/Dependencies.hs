{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Ide.Plugin.Cabal.Dependencies (dependencyVersionHints, collectPackageDependencyVersions, dependencyVersionLens, collectPackageDependencyVersions', printVersion) where

import           Data.Array                        ((!))
import           Data.ByteString                   (ByteString)
import           Data.List
import qualified Data.Maybe                        as Maybe
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as Encoding
import           Data.Version                      (Version (..))
import           Development.IDE.GHC.Compat        (HscEnv, filterUniqMap,
                                                    getUnitInfoMap,
                                                    nonDetEltsUniqMap,
                                                    unitPackageNameString,
                                                    unitPackageVersion)
import qualified Distribution.Fields               as Syntax
import qualified Distribution.Parsec.Position      as Syntax
import qualified Ide.Plugin.Cabal.Completion.Types as Types
import           Language.LSP.Protocol.Types       (CodeLens (..), Command (..),
                                                    InlayHint (..), Range (..),
                                                    type (|?) (..))
import           Text.Regex.TDFA                   (Regex, makeRegex,
                                                    matchAllText)

dependencyVersionLens :: [Syntax.Field Syntax.Position] -> HscEnv -> [CodeLens]
dependencyVersionLens cabalFields = (>>= foo) . groupBy (\(Syntax.Position line1 _, _, _) (Syntax.Position line2 _, _, _) -> line1 == line2) . collectPackageDependencyVersions cabalFields
  where
    foo :: [(Syntax.Position, T.Text, Version)] -> [CodeLens]
    foo []       = []
    foo [single] = [mkCodeLens False single]
    foo multi    = mkCodeLens True <$> multi

    mkCodeLens :: Bool -> (Syntax.Position, T.Text, Version) -> CodeLens
    mkCodeLens includePkgName (pos, pkgName, dependencyVersion) =
      let cPos = Types.cabalPositionToLSPPosition pos
          dependencyText =
            if includePkgName
              then pkgName <> " (" <> printVersion dependencyVersion <> ")"
              else printVersion dependencyVersion
          command = Command dependencyText mempty Nothing
      in  CodeLens
            { _range = Range cPos cPos
            , _command = Just command
            , _data_ = Nothing }

dependencyVersionHints ::  [Syntax.Field Syntax.Position] -> HscEnv -> [InlayHint]
dependencyVersionHints cabalFields = fmap mkHint . collectPackageDependencyVersions cabalFields
  where
    mkHint :: (Syntax.Position, T.Text, Version) -> InlayHint
    mkHint (pos, _, dependencyVersion) =
      InlayHint { _position = Types.cabalPositionToLSPPosition pos
                , _label = InL $ " (" <> printVersion dependencyVersion <> ")"
                , _kind = Nothing
                , _textEdits = Nothing
                , _tooltip = Nothing
                , _paddingLeft = Nothing
                , _paddingRight = Nothing
                , _data_ = Nothing
                }

collectPackageDependencyVersions :: [Syntax.Field Syntax.Position] -> HscEnv -> [(Syntax.Position, T.Text, Version)]
collectPackageDependencyVersions cabalFields hscEnv = cabalFields >>= collectPackageVersions
  where
    lookupPackageVersion pkgName = Maybe.listToMaybe $ nonDetEltsUniqMap $ fmap unitPackageVersion $ filterUniqMap ((==) (T.unpack pkgName) . unitPackageNameString) $ getUnitInfoMap hscEnv

    collectPackageVersions :: Syntax.Field Syntax.Position -> [(Syntax.Position, T.Text, Version)]
    collectPackageVersions (Syntax.Field (Syntax.Name _ "build-depends") pos) = concatMap fieldLinePackageVersions pos
    collectPackageVersions (Syntax.Section _ _ fields) = concatMap collectPackageVersions fields
    collectPackageVersions _ = []

    fieldLinePackageVersions :: Syntax.FieldLine Syntax.Position -> [(Syntax.Position, T.Text, Version)]
    fieldLinePackageVersions (Syntax.FieldLine pos line) =
      let linePackageNameRegex :: Regex = makeRegex ("(^|,)[[:space:]]*([a-zA-Z-]+)" :: ByteString)
          packageNames = (\x -> x ! 2) <$> matchAllText linePackageNameRegex (Encoding.decodeUtf8Lenient line)
          versions = do
            (pkgName, (pkgIndex, pkgOffset)) <- packageNames
            version <- Maybe.maybeToList $ lookupPackageVersion pkgName
            pure (Syntax.Position (Syntax.positionRow pos) (Syntax.positionCol pos + pkgIndex + pkgOffset), pkgName, version)
       in versions

collectPackageDependencyVersions' ::  [Syntax.Field Syntax.Position] -> HscEnv -> [(Range, T.Text, Version)]
collectPackageDependencyVersions' cabalFields hscEnv = cabalFields >>= collectPackageVersions
  where
    lookupPackageVersion pkgName = Maybe.listToMaybe $ nonDetEltsUniqMap $ fmap unitPackageVersion $ filterUniqMap ((==) (T.unpack pkgName) . unitPackageNameString) $ getUnitInfoMap hscEnv

    collectPackageVersions :: Syntax.Field Syntax.Position -> [(Range, T.Text, Version)]
    collectPackageVersions (Syntax.Field (Syntax.Name _ "build-depends") pos) = concatMap fieldLinePackageVersions pos
    collectPackageVersions (Syntax.Section _ _ fields) = concatMap collectPackageVersions fields
    collectPackageVersions _ = []

    fieldLinePackageVersions :: Syntax.FieldLine Syntax.Position -> [(Range, T.Text, Version)]
    fieldLinePackageVersions (Syntax.FieldLine pos line) =
      let linePackageNameRegex :: Regex = makeRegex ("(^|,)[[:space:]]*([a-zA-Z-]+)" :: ByteString)
          packageNames = (\x -> x ! 2) <$> matchAllText linePackageNameRegex (Encoding.decodeUtf8Lenient line)
          versions = do
            (pkgName, (pkgIndex, pkgOffset)) <- packageNames
            version <- Maybe.maybeToList $ lookupPackageVersion pkgName
            let pkgPosStart = Types.cabalPositionToLSPPosition $ Syntax.Position (Syntax.positionRow pos) (Syntax.positionCol pos + pkgIndex)
                pkgPosEnd = Types.cabalPositionToLSPPosition $ Syntax.Position (Syntax.positionRow pos) (Syntax.positionCol pos + pkgIndex + pkgOffset)
            pure (Range pkgPosStart pkgPosEnd, pkgName, version)
       in versions

printVersion :: Version -> T.Text
printVersion v = T.intercalate "." (fmap (T.pack . show) $ versionBranch v)
