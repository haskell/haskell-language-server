{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Ormolu
  (
    provider
  )
where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           Development.IDE.Core.Rules
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import qualified DynFlags as D
import qualified EnumSet  as S
import           GHC
import           Ide.Types
import qualified HIE.Bios as BIOS
import           Ide.Plugin.Formatter
import           Language.Haskell.LSP.Types
import           Ormolu
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

provider :: FormattingProvider IO
#if __GLASGOW_HASKELL__ >= 806
provider ideState typ contents fp _ = do
  let
    exop s =
      "-X" `isPrefixOf` s || "-fplugin=" `isPrefixOf` s || "-pgmF=" `isPrefixOf` s
  opts <- lookupBiosComponentOptions fp
  let cradleOpts =
        map DynOption
          $   filter exop
          $   join
          $   maybeToList
          $   BIOS.componentOptions
          <$> opts
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

  m_parsed <- runAction ideState $ getParsedModule fp
  fileOpts <- case m_parsed of
          Nothing -> return []
          Just pm -> fromDyn pm

  let
    conf o = Config o False False True False
    fmt :: T.Text -> [DynOption] -> IO (Either OrmoluException T.Text)
    fmt cont o =
      try @OrmoluException (ormolu (conf o) (fromNormalizedFilePath fp) $ T.unpack cont)

  case typ of
    FormatText -> ret (fullRange contents) <$> fmt contents cradleOpts
    FormatRange r ->
      let
        txt = T.lines $ extractRange r contents
        lineRange (Range (Position sl _) (Position el _)) =
          Range (Position sl 0) $ Position el $ T.length $ last txt
        hIsSpace (h : _) = T.all isSpace h
        hIsSpace _       = True
        fixS t = if hIsSpace txt && (not $ hIsSpace t) then "" : t else t
        fixE t = if T.all isSpace $ last txt then t else T.init t
        unStrip :: T.Text -> T.Text -> T.Text
        unStrip ws new =
          fixE $ T.unlines $ map (ws `T.append`) $ fixS $ T.lines new
        mStrip :: Maybe (T.Text, T.Text)
        mStrip = case txt of
          (l : _) ->
            let ws = fst $ T.span isSpace l
            in  (,) ws . T.unlines <$> traverse (T.stripPrefix ws) txt
          _ -> Nothing
        err :: IO (Either ResponseError (List TextEdit))
        err = return $ Left $ responseError
          $ T.pack "You must format a whole block of code. Ormolu does not support arbitrary ranges."
        fmt' :: (T.Text, T.Text) -> IO (Either ResponseError (List TextEdit))
        fmt' (ws, striped) =
          ret (lineRange r) <$> (fmap (unStrip ws) <$> fmt striped fileOpts)
      in
        maybe err fmt' mStrip
 where
  ret :: Range -> Either OrmoluException T.Text -> Either ResponseError (List TextEdit)
  ret _ (Left err) = Left
    (responseError (T.pack $ "ormoluCmd: " ++ show err) )
  ret r (Right new) = Right (List [TextEdit r new])

#else
provider _ _ _ _ = return $ Right [] -- NOP formatter
#endif

-- ---------------------------------------------------------------------

-- | Find the cradle wide 'ComponentOptions' that apply to a 'FilePath'
lookupBiosComponentOptions :: (Monad m) => NormalizedFilePath -> m (Maybe BIOS.ComponentOptions)
lookupBiosComponentOptions _fp = do
  -- gmc <- getModuleCache
  -- return $ lookupInCache fp gmc (const Just) (Just . compOpts) Nothing
  return Nothing

-- ---------------------------------------------------------------------
