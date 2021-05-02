{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ide.Plugin.Ormolu
  (
    descriptor
  , provider
  )
where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Text                   as T
import           Development.IDE             hiding (pluginHandlers)
import qualified DynFlags                    as D
import qualified EnumSet                     as S
import           GHC
import           GHC.LanguageExtensions.Type
import           GhcPlugins                  (HscEnv (hsc_dflags))
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server hiding (defaultConfig)
import           Language.LSP.Types
import           "ormolu" Ormolu
import           System.FilePath             (takeFileName)
import           Text.Regex.TDFA.Text        ()

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkFormattingHandlers provider
  }

-- ---------------------------------------------------------------------

provider :: FormattingHandler IdeState
provider ideState typ contents fp _ = withIndefiniteProgress title Cancellable $ liftIO $ do
  let
    fromDyn :: DynFlags -> IO [DynOption]
    fromDyn df =
      let
        pp =
          let p = D.sPgm_F $ D.settings df
          in  ["-pgmF=" <> p | not (null p)]
        pm = map (("-fplugin=" <>) . moduleNameString) $ D.pluginModNames df
        ex = map showExtension $ S.toList $ D.extensionFlags df
      in
        return $ map DynOption $ pp <> pm <> ex

  ghc <- runAction "Ormolu" ideState $ use GhcSession fp
  let df = hsc_dflags . hscEnv <$> ghc
  fileOpts <- case df of
          Nothing -> return []
          Just df -> fromDyn df

  let
    fullRegion = RegionIndices Nothing Nothing
    rangeRegion s e = RegionIndices (Just $ s + 1) (Just $ e + 1)
    mkConf o region = defaultConfig { cfgDynOptions = o,  cfgRegion = region }
    fmt :: T.Text -> Config RegionIndices -> IO (Either OrmoluException T.Text)
    fmt cont conf =
      try @OrmoluException (ormolu conf (fromNormalizedFilePath fp) $ T.unpack cont)

  case typ of
    FormatText -> ret <$> fmt contents (mkConf fileOpts fullRegion)
    FormatRange (Range (Position sl _) (Position el _)) ->
      ret <$> fmt contents (mkConf fileOpts (rangeRegion sl el))
 where
  title = T.pack $ "Formatting " <> takeFileName (fromNormalizedFilePath fp)
  ret :: Either OrmoluException T.Text -> Either ResponseError (List TextEdit)
  ret (Left err) = Left
    (responseError (T.pack $ "ormoluCmd: " ++ show err) )
  ret (Right new) = Right (makeDiffTextEdit contents new)

showExtension :: Extension -> String
showExtension Cpp   = "-XCPP"
showExtension other = "-X" ++ show other
