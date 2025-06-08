{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Cabal.Dependencies (dependencyVersionHints, collectPackageDependencyVersions) where

import qualified Data.Char                         as Char
import qualified Data.List                         as List
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
import           Language.LSP.Protocol.Types       (InlayHint (..),
                                                    InlayHintLabelPart (InlayHintLabelPart),
                                                    type (|?) (..))

dependencyVersionHints ::  [Syntax.Field Syntax.Position] -> HscEnv -> [InlayHint]
dependencyVersionHints cabalFields = fmap mkHint . collectPackageDependencyVersions cabalFields
  where
    mkHint :: (Syntax.Position, Version) -> InlayHint
    mkHint (pos, dependencyVersion) =
      let mkInlayHintLabelPart = InlayHintLabelPart (" (" <> printVersion dependencyVersion <> ")") Nothing Nothing Nothing
      in  InlayHint { _position = Types.cabalPositionToLSPPosition pos
                    , _label = InR $ pure mkInlayHintLabelPart
                    , _kind = Nothing
                    , _textEdits = Nothing
                    , _tooltip = Nothing
                    , _paddingLeft = Nothing
                    , _paddingRight = Nothing
                    , _data_ = Nothing
                    }

collectPackageDependencyVersions :: [Syntax.Field Syntax.Position] -> HscEnv -> [(Syntax.Position, Version)]
collectPackageDependencyVersions cabalFields hscEnv = cabalFields >>= collectPackageVersions
  where
    lookupPackageVersion pkgName = Maybe.listToMaybe $ nonDetEltsUniqMap $ fmap unitPackageVersion $ filterUniqMap ((==) (T.unpack pkgName) . unitPackageNameString) $ getUnitInfoMap hscEnv

    collectPackageVersions :: Syntax.Field Syntax.Position -> [(Syntax.Position, Version)]
    collectPackageVersions (Syntax.Field (Syntax.Name _ "build-depends") pos) = concatMap fieldLinePackageVersions pos
    collectPackageVersions (Syntax.Section _ _ fields) = concatMap collectPackageVersions fields
    collectPackageVersions _ = []

    fieldLinePackageVersions :: Syntax.FieldLine Syntax.Position -> [(Syntax.Position, Version)]
    fieldLinePackageVersions (Syntax.FieldLine pos x) =
      let splitted = T.splitOn "," $ Encoding.decodeUtf8Lenient x
          calcStartPosition (prev, start) = T.length prev + 1 + start
          potentialPkgs = List.foldl' (\a b -> a <> [(b, Maybe.maybe 0 calcStartPosition $ Maybe.listToMaybe $ reverse a)]) [] splitted
          versions = do
            (pkg', pkgStartOffset) <- potentialPkgs
            let pkgName = T.takeWhile (not . Char.isSpace) . T.strip $ pkg'
                endOfPackage = T.length pkgName + (T.length $ T.takeWhile Char.isSpace pkg')
            version <- Maybe.maybeToList $ lookupPackageVersion $ T.takeWhile (not . Char.isSpace) . T.strip $ pkg'
            pure (Syntax.Position (Syntax.positionRow pos) (Syntax.positionCol pos + pkgStartOffset + endOfPackage), version)
       in versions

printVersion :: Version -> T.Text
printVersion v = T.intercalate "." (fmap (T.pack . show) $ versionBranch v)
