{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.Cabal.Rules (cabalRules, Log) where

import           Control.Monad.IO.Class
import qualified Data.ByteString                   as BS
import qualified Data.List                         as List
import qualified Data.List.NonEmpty                as NE
import qualified Data.Maybe                        as Maybe
import qualified Data.Text                         ()
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as Encoding
import           Data.Text.Utf16.Rope.Mixed        as Rope
import           Development.IDE                   as D
import qualified Development.IDE.Core.Shake        as Shake
import qualified Distribution.CabalSpecVersion     as Cabal
import qualified Distribution.Fields               as Syntax
import           Distribution.Parsec.Error
import qualified Ide.Plugin.Cabal.Completion.Data  as Data
import           Ide.Plugin.Cabal.Completion.Types (ParseCabalCommonSections (ParseCabalCommonSections),
                                                    ParseCabalFields (..),
                                                    ParseCabalFile (..))
import qualified Ide.Plugin.Cabal.Diagnostics      as Diagnostics
import qualified Ide.Plugin.Cabal.OfInterest       as OfInterest
import           Ide.Plugin.Cabal.Orphans          ()
import qualified Ide.Plugin.Cabal.Parse            as Parse
import           Ide.Types
import           Text.Regex.TDFA

data Log
  = LogModificationTime NormalizedFilePath FileVersion
  | LogShake Shake.Log
  | LogOfInterest OfInterest.Log
  | LogDocSaved Uri
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogShake log' -> pretty log'
    LogOfInterest log' -> pretty log'
    LogModificationTime nfp modTime ->
      "Modified:" <+> pretty (fromNormalizedFilePath nfp) <+> pretty (show modTime)
    LogDocSaved uri ->
      "Saved text document:" <+> pretty (getUri uri)

cabalRules :: Recorder (WithPriority Log) -> PluginId -> Rules ()
cabalRules recorder plId = do
  -- Make sure we initialise the cabal files-of-interest.
  OfInterest.ofInterestRules (cmapWithPrio LogOfInterest recorder)
  -- Rule to produce diagnostics for cabal files.
  define (cmapWithPrio LogShake recorder) $ \ParseCabalFields file -> do
    config <- getPluginConfigAction plId
    if not (plcGlobalOn config && plcDiagnosticsOn config)
      then pure ([], Nothing)
      else do
        -- whenever this key is marked as dirty (e.g., when a user writes stuff to it),
        -- we rerun this rule because this rule *depends* on GetModificationTime.
        (t, mCabalSource) <- use_ GetFileContents file
        log' Debug $ LogModificationTime file t
        contents <- case mCabalSource of
          Just sources ->
            pure $ Encoding.encodeUtf8 $ Rope.toText sources
          Nothing -> do
            liftIO $ BS.readFile $ fromNormalizedFilePath file

        case Parse.readCabalFields file contents of
          Left _ ->
            pure ([], Nothing)
          Right fields ->
            pure ([], Just fields)

  define (cmapWithPrio LogShake recorder) $ \ParseCabalCommonSections file -> do
    fields <- use_ ParseCabalFields file
    let commonSections =
          Maybe.mapMaybe
            ( \case
                commonSection@(Syntax.Section (Syntax.Name _ "common") _ _) -> Just commonSection
                _ -> Nothing
            )
            fields
    pure ([], Just commonSections)

  define (cmapWithPrio LogShake recorder) $ \ParseCabalFile file -> do
    config <- getPluginConfigAction plId
    if not (plcGlobalOn config && plcDiagnosticsOn config)
      then pure ([], Nothing)
      else do
        -- whenever this key is marked as dirty (e.g., when a user writes stuff to it),
        -- we rerun this rule because this rule *depends* on GetModificationTime.
        (t, mCabalSource) <- use_ GetFileContents file
        log' Debug $ LogModificationTime file t
        contents <- case mCabalSource of
          Just sources ->
            pure $ Encoding.encodeUtf8 $ Rope.toText sources
          Nothing -> do
            liftIO $ BS.readFile $ fromNormalizedFilePath file

        -- Instead of fully reparsing the sources to get a 'GenericPackageDescription',
        -- we would much rather re-use the already parsed results of 'ParseCabalFields'.
        -- Unfortunately, Cabal-syntax doesn't expose the function 'parseGenericPackageDescription''
        -- which allows us to resume the parsing pipeline with '[Field Position]'.
        let (pWarnings, pm) = Parse.parseCabalFileContents contents
        let warningDiags = fmap (Diagnostics.warningDiagnostic file) pWarnings
        case pm of
          Left (_cabalVersion, pErrorNE) -> do
            let regexUnknownCabalBefore310 :: T.Text
                -- We don't support the cabal version, this should not be an error, as the
                -- user did not do anything wrong. Instead we cast it to a warning
                regexUnknownCabalBefore310 = "Unsupported cabal-version [0-9]+.[0-9]*"
                regexUnknownCabalVersion :: T.Text
                regexUnknownCabalVersion = "Unsupported cabal format version in cabal-version field: [0-9]+.[0-9]+"
                unsupportedCabalHelpText =
                  unlines
                    [ "The used `cabal-version` is not fully supported by this `HLS` binary."
                    , "Either the `cabal-version` is unknown, or too new for this executable."
                    , "This means that some functionality might not work as expected."
                    , "If you face any issues, try downgrading to a supported `cabal-version` or upgrading `HLS` if possible."
                    , ""
                    , "Supported versions are: "
                        <> List.intercalate
                          ", "
                          (fmap Cabal.showCabalSpecVersion Data.supportedCabalVersions)
                    ]
                errorDiags =
                  NE.toList $
                    NE.map
                      ( \pe@(PError pos text) ->
                          if any
                            (text =~)
                            [ regexUnknownCabalBefore310
                            , regexUnknownCabalVersion
                            ]
                            then
                              Diagnostics.warningDiagnostic
                                file
                                ( Syntax.PWarning Syntax.PWTOther pos $
                                    unlines
                                      [ text
                                      , unsupportedCabalHelpText
                                      ]
                                )
                            else Diagnostics.errorDiagnostic file pe
                      )
                      pErrorNE
                allDiags = errorDiags <> warningDiags
            pure (allDiags, Nothing)
          Right gpd -> do
            pure (warningDiags, Just gpd)

  action $ do
    -- Run the cabal kick. This code always runs when 'shakeRestart' is run.
    -- Must be careful to not impede the performance too much. Crucial to
    -- a snappy IDE experience.
    OfInterest.kick
 where
  log' = logWith recorder
