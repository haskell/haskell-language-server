{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

module Ide.Plugin.ExplicitFixity where

import           Control.Monad                 (forM)
import           Control.Monad.IO.Class        (liftIO)
import           Data.List.Extra               (nubOn)
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Text                     as T
import           Development.IDE               hiding (pluginHandlers)
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint (pointCommand)
import           Ide.PluginUtils               (getNormalizedFilePath,
                                                handleMaybeM, pluginResponse)
import           Ide.Types                     hiding (pluginId)
import           Language.LSP.Types

pluginId :: PluginId
pluginId = "explicitFixity"

descriptor :: PluginDescriptor IdeState
descriptor = (defaultPluginDescriptor pluginId)
    { pluginHandlers = mkPluginHandler STextDocumentHover hover
    }

hover :: PluginMethodHandler IdeState TextDocumentHover
hover state plId (HoverParams (TextDocumentIdentifier uri) pos _) = pluginResponse $ do
    nfp <- getNormalizedFilePath plId uri
    har <- handleMaybeM "Unable to get HieResult"
        $ liftIO
        $ runAction "ExplicitFixity.HieResult" state
        $ use GetHieAst nfp

    -- Names at the position
    let names = case har of
            HAR _ asts _ _ _ -> concat
                $ pointCommand asts pos
                $ \ast -> mapMaybe (either (const Nothing) Just) $ M.keys $ getNodeIds ast

    -- Get fixity from HscEnv for local defined operator will crash the plugin,
    -- we first try to use ModIface to get fixities, and then use
    -- HscEnv if ModIface doesn't available.
    fixities <- getFixityFromModIface nfp names

    if isJust fixities then pure fixities else getFixityFromEnv nfp names
    where
        -- | For local definition
        getFixityFromModIface nfp names = do
            hi <- handleMaybeM "Unable to get ModIface"
                $ liftIO
                $ runAction "ExplicitFixity.GetModIface" state
                $ use GetModIface nfp
            let iface = hirModIface hi
                fixities = filter (\(_, fixity) -> fixity /= defaultFixity)
                    $ map (\name -> (name, mi_fix iface (occName name))) names
            -- We don't have much fixities on one position,
            -- so `nubOn` is acceptable.
            pure $ toHover $ nubOn snd fixities

        -- | Get fixity from HscEnv
        getFixityFromEnv nfp names = do
            env <- fmap hscEnv
                $ handleMaybeM "Unable to get GhcSession"
                $ liftIO
                $ runAction "ExplicitFixity.GhcSession" state
                $ use GhcSession nfp
            fixities <- liftIO
                $ fmap (filter ((/= defaultFixity) . snd) . mapMaybe cond)
                $ forM names $ \name ->
                    (\(_, fixity) -> (name, fixity)) <$> runTcInteractive env (lookupFixityRn name)
            pure $ toHover $ nubOn snd fixities

            where
                cond (_, Nothing) = Nothing
                cond (name, Just f) = Just (name, f)

        toHover [] = Nothing
        toHover fixities =
            let contents = T.intercalate "\n\n" $ fixityText <$> fixities
                contents' = "\n" <> sectionSeparator <> contents
            in  Just $ Hover (HoverContents $ unmarkedUpContent contents') Nothing

        fixityText (name, Fixity _ precedence direction) =
            printOutputable direction <> " " <> printOutputable precedence <> " `" <> printOutputable name <> "`"
