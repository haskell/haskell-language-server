{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Fourmolu.Common where

import           Data.List             (intercalate)
import           Data.Text             (Text)
import           Development.IDE       hiding (pluginHandlers)
import           Ide.Plugin.Properties

data LogEvent
    = NoVersion Text
    | ConfigPath FilePath
    | NoConfigPath FilePath [FilePath]
    | StdErr Text
    | LogCompiledInVersion String
    | LogExternalVersion [Int]
    deriving (Show)

instance Pretty LogEvent where
    pretty = \case
        NoVersion t -> "Couldn't get Fourmolu version:" <> line <> indent 2 (pretty t)
        ConfigPath p -> "Loaded Fourmolu config from: " <> pretty (show p)
        NoConfigPath expected ps -> "No " <> pretty expected <> " found in any of:"
            <> line <> indent 2 (vsep (map (pretty . show) ps))
        StdErr t -> "Fourmolu stderr:" <> line <> indent 2 (pretty t)
        LogCompiledInVersion v -> "Using compiled in fourmolu-" <> pretty v
        LogExternalVersion v ->
            "Using external fourmolu"
            <> if null v then "" else "-"
            <> pretty (intercalate "." $ map show v)

properties :: Properties '[ 'PropertyKey "external" 'TBoolean, 'PropertyKey "path" 'TString]
properties =
    emptyProperties
        & defineStringProperty
            #path
            "Set path to executable (for \"external\" mode)."
            "fourmolu"
        & defineBooleanProperty
            #external
            "Call out to an external \"fourmolu\" executable, rather than using the bundled library."
            False

pluginDescMain :: Text
pluginDescMain = "Provides formatting of Haskell files via fourmolu."
