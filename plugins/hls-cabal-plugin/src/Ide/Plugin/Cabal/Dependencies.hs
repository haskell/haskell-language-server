{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Ide.Plugin.Cabal.Dependencies (dependencyVersionHints, dependencyVersionLens, dependencyHover) where

import           Control.Lens                        (to, (^.))
import           Data.Array                          ((!))
import           Data.ByteString                     (ByteString)
import           Data.List
import qualified Data.List                           as List
import qualified Data.Maybe                          as Maybe
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as Encoding
import           Data.Version                        (Version (..))
import           Development.IDE.GHC.Compat          (HscEnv, filterUniqMap,
                                                      getUnitInfoMap,
                                                      nonDetEltsUniqMap,
                                                      unitPackageNameString,
                                                      unitPackageVersion)
import           Development.IDE.LSP.HoverDefinition (foundHover)
import qualified Distribution.Fields                 as Syntax
import qualified Distribution.Parsec.Position        as Syntax
import qualified Ide.Plugin.Cabal.Completion.Types   as Types
import qualified Language.LSP.Protocol.Lens          as JL
import           Language.LSP.Protocol.Types         (CodeLens (..),
                                                      Command (..), Hover,
                                                      InlayHint (..), Null (..),
                                                      Position, Range (..),
                                                      positionInRange,
                                                      type (|?) (..))
import           Text.Regex.TDFA                     (Regex, makeRegex,
                                                      matchAllText)

dependencyVersionLens :: [Syntax.Field Syntax.Position] -> HscEnv -> [CodeLens]
dependencyVersionLens cabalFields = Maybe.catMaybes . (>>= foo) . groupBy (\a b-> (a ^. to range . JL.start . JL.line) == (b ^. to range . JL.start . JL.line)) . collectPackageDependencyVersions cabalFields
  where
    foo :: [DependencyInfo] -> [Maybe CodeLens]
    foo []       = []
    foo [single] = [mkCodeLens False single]
    foo multi    = mkCodeLens True <$> multi

    mkCodeLens :: Bool -> DependencyInfo -> Maybe CodeLens
    mkCodeLens includePkgName DependencyInfo{range, packageName, installedVersion = Just version} =
      let dependencyText =
            if includePkgName
              then packageName <> " (" <> printVersion version <> ")"
              else printVersion version
          command = Command dependencyText mempty Nothing
      in  Just $ CodeLens
            { _range = range
            , _command = Just command
            , _data_ = Nothing }
    mkCodeLens _ _ = Nothing

dependencyVersionHints ::  [Syntax.Field Syntax.Position] -> HscEnv -> [InlayHint]
dependencyVersionHints cabalFields = Maybe.mapMaybe mkHint . collectPackageDependencyVersions cabalFields
  where
    mkHint :: DependencyInfo -> Maybe InlayHint
    mkHint (DependencyInfo range _ (Just installedVersion)) =
      Just $
        InlayHint { _position = range ^. JL.end
                  , _label = InL $ " (" <> printVersion installedVersion <> ")"
                  , _kind = Nothing
                  , _textEdits = Nothing
                  , _tooltip = Nothing
                  , _paddingLeft = Nothing
                  , _paddingRight = Nothing
                  , _data_ = Nothing
                  }
    mkHint _ = Nothing

dependencyHover :: [Syntax.Field Syntax.Position] -> HscEnv -> Position -> Hover |? Null
dependencyHover cabalFields hsc cursorPosition =
  let hoveredDep = List.find (positionInRange cursorPosition . range) $ collectPackageDependencyVersions cabalFields hsc
  in  case hoveredDep of
    Just (DependencyInfo {packageName, installedVersion}) ->
      let showVersion f = maybe T.empty (f . printVersion) installedVersion
      in  foundHover (Nothing, [packageName <> showVersion (\v -> " (" <> v <> ")") <> "\n", documentationText (packageName <> showVersion ("-" <>))])
    Nothing -> InR Null
  where
  documentationText :: T.Text -> T.Text
  documentationText package = "[Documentation](https://hackage.haskell.org/package/" <> package <> ")"

collectPackageDependencyVersions ::  [Syntax.Field Syntax.Position] -> HscEnv -> [DependencyInfo]
collectPackageDependencyVersions cabalFields hscEnv = cabalFields >>= collectPackageVersions
  where
    lookupPackageVersion pkgName = Maybe.listToMaybe $ nonDetEltsUniqMap $ fmap unitPackageVersion $ filterUniqMap ((==) (T.unpack pkgName) . unitPackageNameString) $ getUnitInfoMap hscEnv

    collectPackageVersions :: Syntax.Field Syntax.Position -> [DependencyInfo]
    collectPackageVersions (Syntax.Field (Syntax.Name _ "build-depends") pos) = concatMap fieldLinePackageVersions pos
    collectPackageVersions (Syntax.Section _ _ fields) = concatMap collectPackageVersions fields
    collectPackageVersions _ = []

    fieldLinePackageVersions :: Syntax.FieldLine Syntax.Position -> [DependencyInfo]
    fieldLinePackageVersions (Syntax.FieldLine pos line) =
      let linePackageNameRegex :: Regex = makeRegex ("(^|,)[[:space:]]*([a-zA-Z-]+)" :: ByteString)
          packageNames = (\x -> x ! 2) <$> matchAllText linePackageNameRegex (Encoding.decodeUtf8Lenient line)
          versions = do
            (pkgName, (pkgIndex, pkgOffset)) <- packageNames
            let pkgPosStart = Types.cabalPositionToLSPPosition $ Syntax.Position (Syntax.positionRow pos) (Syntax.positionCol pos + pkgIndex)
                pkgPosEnd = Types.cabalPositionToLSPPosition $ Syntax.Position (Syntax.positionRow pos) (Syntax.positionCol pos + pkgIndex + pkgOffset)
                version = lookupPackageVersion pkgName
            pure $ DependencyInfo (Range pkgPosStart pkgPosEnd) pkgName version
       in versions

data DependencyInfo = DependencyInfo
  { range            :: Range
  , packageName      :: T.Text
  , installedVersion :: Maybe Version
  }

printVersion :: Version -> T.Text
printVersion v = T.intercalate "." (fmap (T.pack . show) $ versionBranch v)
