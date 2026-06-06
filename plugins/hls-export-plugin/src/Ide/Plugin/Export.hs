{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Export (descriptor) where

import           Control.Concurrent.STM           (atomically)
import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils (runActionE, useE)
import           Development.IDE.Core.Shake       (getDiagnostics)
import           Development.IDE.GHC.Compat
-- TcRnUnusedName (and its provenance) only became a structured diagnostic in
-- GHC 9.8. On 9.6 we cannot single out unused top-level binds this way.
#if MIN_VERSION_ghc(9,8,0)
import           Development.IDE.GHC.Compat.Error (_TcRnMessage,
                                                   msgEnvelopeErrorL)
import           GHC.Tc.Errors.Types              (TcRnMessage (TcRnUnusedName),
                                                   UnusedNameProv (UnusedNameTopDecl))
#endif
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
  pm <- runActionE "Export.GetParsedModuleWithComments" state (useE GetParsedModuleWithComments nfp)
  let ps = pm_parsed_source pm
  case (isExplicit ps, locateUnderCursor (range ^. L.start) ps) of
    (True, Just under) -> do
      -- The names GHC flags as defined-but-unused. Attach the action to the
      -- unused diagnostics as well.
      unusedDiags <- liftIO $ unusedTopBindDiagnostics state nfp
      pure . InL . map InR $
        [ ca
        | Just (verb, title, edits) <-
            [ addAction under ps
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
#if MIN_VERSION_ghc(9,8,0)
    isUnusedTopBind d =
      case d ^? fdStructuredMessageL . _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnMessage of
        Just (TcRnUnusedName _ UnusedNameTopDecl) -> True
        _                                         -> False
#else
    isUnusedTopBind _ = False
#endif

addAction :: UnderCursor -> ParsedSource -> Maybe (Text, Text, [TextEdit])
addAction under ps = case under of
  Decl flavor n
    | n `isExported` ps -> Nothing
    | otherwise -> ("Export", T.pack (printRdrName n),) <$> addExport ps (mkExportIE flavor n)
  Constructor t c
    | c `isExported` ps -> Nothing
    | otherwise ->
        ("Export", T.pack (printRdrName t) <> "(" <> T.pack (printRdrName c) <> ")",)
          <$> addConstructorExport t c ps
  Header -> Nothing
