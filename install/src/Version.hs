module Version where

import           Data.Version                             ( Version
                                                          , parseVersion
                                                          , makeVersion
                                                          , showVersion
                                                          )
import           Text.ParserCombinators.ReadP             ( readP_to_S )


type VersionNumber = String
type RequiredVersion = [Int]

versionToString :: RequiredVersion -> String
versionToString = showVersion . makeVersion

-- | Parse a version-string into a version. Fails if the version-string is not valid
parseVersionEx :: String -> Version
parseVersionEx = fst . head . filter (("" ==) . snd) . readP_to_S parseVersion

-- | Check that a given version-string is not smaller than the required version
checkVersion :: RequiredVersion -> String -> Bool
checkVersion required given = parseVersionEx given >= makeVersion required
