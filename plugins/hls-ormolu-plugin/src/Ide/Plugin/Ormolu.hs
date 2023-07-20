{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Ide.Plugin.Ormolu
  ( descriptor
  , provider
  )
where

import           Control.Exception               (Handler (..), IOException,
                                                  SomeException (..), catches)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Functor                    ((<&>))
import qualified Data.Text                       as T
import           Development.IDE                 hiding (pluginHandlers)
import           Development.IDE.GHC.Compat      (hsc_dflags, moduleNameString)
import qualified Development.IDE.GHC.Compat      as D
import qualified Development.IDE.GHC.Compat.Util as S
import           GHC.LanguageExtensions.Type
import           Ide.Plugin.Error                (PluginError (PluginInternalError))
import           Ide.PluginUtils
import           Ide.Types                       hiding (Config)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server             hiding (defaultConfig)
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
    fmt :: T.Text -> Config RegionIndices -> IO (Either SomeException T.Text)
    fmt cont conf = flip catches handlers $ do
      let fp' = fromNormalizedFilePath fp
#if MIN_VERSION_ormolu(0,5,3)
      cabalInfo <- getCabalInfoForSourceFile fp' <&> \case
        CabalNotFound                -> Nothing
        CabalDidNotMention cabalInfo -> Just cabalInfo
        CabalFound cabalInfo         -> Just cabalInfo
#if MIN_VERSION_ormolu(0,7,0)
      (fixityOverrides, moduleReexports) <- getDotOrmoluForSourceFile fp'
      let conf' = refineConfig ModuleSource cabalInfo (Just fixityOverrides) (Just moduleReexports) conf
#else
      fixityOverrides <- traverse getFixityOverridesForSourceFile cabalInfo
      let conf' = refineConfig ModuleSource cabalInfo fixityOverrides conf
#endif
      let cont' = cont
#else
      let conf' = conf
          cont' = T.unpack cont
#endif
      Right <$> ormolu conf' fp' cont'
    handlers =
      [ Handler $ pure . Left . SomeException @OrmoluException
      , Handler $ pure . Left . SomeException @IOException
      ]

  case typ of
    FormatText -> ret <$> fmt contents (mkConf fileOpts fullRegion)
    FormatRange (Range (Position sl _) (Position el _)) ->
      ret <$> fmt contents (mkConf fileOpts (rangeRegion (fromIntegral sl) (fromIntegral el)))
 where
   title = T.pack $ "Formatting " <> takeFileName (fromNormalizedFilePath fp)

   ret :: Either SomeException T.Text -> Either PluginError ([TextEdit] |? Null)
   ret (Left err)  = Left . PluginInternalError . T.pack $ "ormoluCmd: " ++ show err
   ret (Right new) = Right $ InL $ makeDiffTextEdit contents new

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
