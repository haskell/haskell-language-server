{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Ide.Plugin.Ormolu
  ( descriptor
  , provider
  )
where

import           Control.Exception               (try)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Text                       as T
import           Development.IDE                 hiding (pluginHandlers)
import           Development.IDE.GHC.Compat      (hsc_dflags, moduleNameString)
import qualified Development.IDE.GHC.Compat      as D
import qualified Development.IDE.GHC.Compat.Util as S
import           GHC.LanguageExtensions.Type
import           Ide.PluginUtils
import           Ide.Types                       hiding (Config)
import           Language.LSP.Server             hiding (defaultConfig)
import           Language.LSP.Types
import           Ormolu
import           System.FilePath                 (takeFileName)

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkFormattingHandlers provider
  }

-- ---------------------------------------------------------------------

provider :: FormattingHandler IdeState
provider ideState typ contents fp _ = withIndefiniteProgress title Cancellable $ liftIO $ do
  ghc <- runAction "Ormolu" ideState $ use GhcSession fp
  let df = hsc_dflags . hscEnv <$> ghc
  fileOpts <- case df of
    Nothing -> pure []
    Just df -> pure $ fromDyn df

  let
    fullRegion = RegionIndices Nothing Nothing
    rangeRegion s e = RegionIndices (Just $ s + 1) (Just $ e + 1)
    mkConf o region = defaultConfig { cfgDynOptions = o, cfgRegion = region }
    fmt :: T.Text -> Config RegionIndices -> IO (Either OrmoluException T.Text)
    fmt cont conf =
      try @OrmoluException $ ormolu conf (fromNormalizedFilePath fp) $ T.unpack cont

  case typ of
    FormatText -> ret <$> fmt contents (mkConf fileOpts fullRegion)
    FormatRange (Range (Position sl _) (Position el _)) ->
      ret <$> fmt contents (mkConf fileOpts (rangeRegion (fromIntegral sl) (fromIntegral el)))
 where
   title = T.pack $ "Formatting " <> takeFileName (fromNormalizedFilePath fp)

   ret :: Either OrmoluException T.Text -> Either ResponseError (List TextEdit)
   ret (Left err)  = Left . responseError . T.pack $ "ormoluCmd: " ++ show err
   ret (Right new) = Right $ makeDiffTextEdit contents new

   fromDyn :: D.DynFlags -> [DynOption]
   fromDyn df =
     let
       pp =
         let p = D.sPgm_F $ D.settings df
         in  ["-pgmF=" <> p | not (null p)]
       pm = ("-fplugin=" <>) . moduleNameString <$> D.pluginModNames df
       ex = showExtension <$> S.toList (D.extensionFlags df)
     in
       DynOption <$> pp <> pm <> ex

showExtension :: Extension -> String
showExtension Cpp   = "-XCPP"
showExtension other = "-X" ++ show other
