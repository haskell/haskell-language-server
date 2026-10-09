{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Export (descriptor) where

import           Control.Applicative              ((<|>))
import           Control.Concurrent.STM           (atomically)
import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import           Data.Maybe                       (isJust, isNothing)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Utf16.Rope.Mixed       (Rope)
import           Development.IDE
import           Development.IDE.Core.PluginUtils (runActionE, useE)
import           Development.IDE.Core.Shake       (getDiagnostics)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error (_TcRnUnusedTopBind,
                                                   msgEnvelopeErrorL)
import qualified GHC.LanguageExtensions.Type      as LangExt (Extension (..))
import           Ide.Plugin.Error                 (getNormalizedFilePathE)
import           Ide.Plugin.Export.Cursor
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Exports
import           Ide.Plugin.Export.Utils
import           Ide.Types
import qualified Ide.Types                        as Ide
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message    (Method (..), SMethod (..))
import           Language.LSP.Protocol.Types

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  let exportHandlers = mkPluginHandler SMethod_TextDocumentCodeAction quickCodeActionHandlers
  in (defaultPluginDescriptor plId "Code actions for module export lists")
    { Ide.pluginHandlers = exportHandlers
    }

quickCodeActionHandlers :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
quickCodeActionHandlers state _plId (CodeActionParams _ _ doc range _) = do
  let uri = doc ^. L.uri
  nfp <- getNormalizedFilePathE uri
  (ps, isCpp, mUnder, msrc) <- runActionE "Export.getInputs" state $ do
    pm <- useE GetParsedModuleWithComments nfp
    let ps = pm_parsed_source pm
        isCpp = xopt LangExt.Cpp (ms_hspp_opts (pm_mod_summary pm))
        mUnder = if isExplicit ps then locateUnderCursor (range ^. L.start) ps else Nothing
    -- Only a CPP module about to be offered an action needs the buffer (to find
    -- directives in the export list), so skip the fetch otherwise.
    msrc <- if isJust mUnder && isCpp then snd <$> useE GetFileContents nfp else pure Nothing
    pure (ps, isCpp, mUnder, msrc)
  case mUnder of
    -- A CPP module whose buffer we could not read may have directives in the
    -- export list that a reprint would silently erase. Withhold rather than risk
    -- it.
    Just under | not (isCpp && isNothing msrc) -> do
      -- The names GHC flags as defined-but-unused. Attach the action to the
      -- unused diagnostics as well.
      unusedDiags <- liftIO $ unusedTopBindDiagnostics state nfp
      pure . InL . map InR $
        [ ca
        | Just (verb, title, edits) <-
            [ addAction msrc under ps
            , removeAction msrc under ps
            ]
        , let fixes = [ d | d <- unusedDiags, locateUnderCursor (d ^. L.range . L.start) ps == Just under ]
              ca = mkAction (verb <> " `" <> title <> "`")
                     & L.edit ?~ singleFileEdit uri edits
                     & L.diagnostics .~ (if null fixes then Nothing else Just fixes)
        ]
    _ -> pure (InL [])

-- | The LSP diagnostics for names GHC reports as unused top-level definitions.
unusedTopBindDiagnostics :: IdeState -> NormalizedFilePath -> IO [Diagnostic]
unusedTopBindDiagnostics state nfp = do
  diags <- atomically $ getDiagnostics state
  pure [ fdLspDiagnostic d | d <- diags, fdFilePath d == nfp, isUnusedTopBind d ]
  where
    isUnusedTopBind =
      has (fdStructuredMessageL . _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnUnusedTopBind)

addAction :: Maybe Rope -> UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
addAction msrc under ps = case under of
  Decl flavor n
    | n `isExported` ps -> Nothing
    | otherwise -> ("Export", T.pack (printRdrName n),) <$> addExport msrc ps (mkExportIE flavor n)
  Constructor t c
    | c `isExported` ps -> Nothing
    | otherwise ->
        ("Export", T.pack (printRdrName t) <> "(" <> T.pack (printRdrName c) <> ")",)
          <$> addConstructorExport msrc t c ps
  Header -> Nothing

removeAction :: Maybe Rope -> UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
removeAction msrc under ps = case under of
  Decl _ n -> ("Unexport", T.pack (printRdrName n),) <$> removeExport msrc ps n
  -- A bare uppercase entry denotes the type, so when the constructor shares the
  -- type's name, skip the standalone-removal fallback.
  Constructor t c ->
    ("Unexport", T.pack (printRdrName c),) <$>
      (removeConstructorExport msrc t c ps
        <|> if rdrNameFS c == rdrNameFS t then Nothing else removeExport msrc ps c)
  Header -> Nothing
