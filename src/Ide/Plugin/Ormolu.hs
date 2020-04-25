{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Ormolu
  (
    descriptor
  , provider
  )
where

import           Control.Exception
import qualified Data.Text as T
import           Development.IDE.Core.Rules
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import qualified DynFlags as D
import qualified EnumSet  as S
import           GHC
import           Ide.Types
import           Ide.PluginUtils
import           Ide.Plugin.Formatter
import           Language.Haskell.LSP.Types
import           Ormolu
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginRules = mempty
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolsProvider    = Nothing
  , pluginFormattingProvider = Just provider
  , pluginCompletionProvider = Nothing
  }

-- ---------------------------------------------------------------------

provider :: FormattingProvider IO
provider _lf ideState typ contents fp _ = do
  let
    fromDyn :: ParsedModule -> IO [DynOption]
    fromDyn pmod =
      let
        df = ms_hspp_opts $ pm_mod_summary pmod
        pp =
          let p = D.sPgm_F $ D.settings df
          in  if null p then [] else ["-pgmF=" <> p]
        pm = map (("-fplugin=" <>) . moduleNameString) $ D.pluginModNames df
        ex = map (("-X" <>) . show) $ S.toList $ D.extensionFlags df
      in
        return $ map DynOption $ pp <> pm <> ex

  m_parsed <- runAction (fromNormalizedFilePath fp) ideState $ getParsedModule fp
  fileOpts <- case m_parsed of
          Nothing -> return []
          Just pm -> fromDyn pm

  let
    fullRegion = RegionIndices Nothing Nothing
    rangeRegion s e = RegionIndices (Just s) (Just e)
    mkConf o region = defaultConfig { cfgDynOptions = o,  cfgRegion = region }
    fmt :: T.Text -> Config RegionIndices -> IO (Either OrmoluException T.Text)
    fmt cont conf =
      try @OrmoluException (ormolu conf (fromNormalizedFilePath fp) $ T.unpack cont)

  case typ of
    FormatText -> ret <$> fmt contents (mkConf fileOpts fullRegion)
    FormatRange r ->
      let
        Range (Position sl _) (Position el _) = normalize r
      in
        ret <$> fmt contents (mkConf fileOpts (rangeRegion sl el))
 where
  ret :: Either OrmoluException T.Text -> Either ResponseError (List TextEdit)
  ret (Left err) = Left
    (responseError (T.pack $ "ormoluCmd: " ++ show err) )
  ret (Right new) = Right (makeDiffTextEdit contents new)
