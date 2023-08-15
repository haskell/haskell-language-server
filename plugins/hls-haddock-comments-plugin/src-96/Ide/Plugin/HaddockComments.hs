{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.HaddockComments (descriptor, E.Log) where


import           Control.Monad                          (join)
import           Control.Monad.IO.Class
import qualified Data.Map                               as Map
import qualified Data.Text                              as T
import           Development.IDE                        hiding (pluginHandlers)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.ExactPrint         (GetAnnotatedParsedSource (..))
import qualified Development.IDE.GHC.ExactPrint         as E
import           Development.IDE.Plugin.CodeAction
import           Data.Maybe                             (catMaybes)
import           Development.IDE.Plugin.CodeAction.Util (traceAst)
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = mkExactprintPluginDescriptor recorder $
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction codeActionProvider
    }

codeActionProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeActionProvider ideState _pId (CodeActionParams _ _ (TextDocumentIdentifier uri) selectionRange CodeActionContext {_diagnostics = diags}) =
    if noErr
      then do
        (fmap astA . join -> pm) <- liftIO $ runAction "HaddockComments.GetAnnotatedParsedSource" ideState $ use GetAnnotatedParsedSource `traverse` nfp
        case pm of
          Nothing                    -> respondEmpty
          Just (pm' :: ParsedSource) ->
            let topLevelDecls = hsmodDecls . unLoc $ pm'
                codeActions = catMaybes
                  [ runGenerator generator decl selectionRange uri |
                    decl <- topLevelDecls,
                    generator <- registeredGenerators
                  ]
             in pure . Right . InL . fmap InR $ codeActions
      else respondEmpty
  where
    noErr = notElem (Just DiagnosticSeverity_Error) . fmap _severity $ diags
    nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    respondEmpty = pure . Right $ InL []

-- | Interface for defining how to generate a code action for a top-level declaration.
data Generator = Generator
  { title     :: T.Text
  , updateAst :: LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)
  }

runGenerator :: Generator -> LHsDecl GhcPs -> Range -> Uri -> Maybe CodeAction
runGenerator generator decl@(L annDecl _) selectionRange uri
  | selectionRange `isIntersectWith` locA annDecl = do
      modifiedDecl <- updateAst generator decl
      range' <- toRange (locA annDecl)
      pure CodeAction {
        _title = title generator,
        _kind = Just CodeActionKind_QuickFix,
        _diagnostics = Nothing,
        _command = Nothing,
        _edit = Just WorkspaceEdit
          { _changes = Just $ Map.singleton uri [TextEdit range' (T.pack $ exactPrint modifiedDecl)]
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          },
        _isPreferred = Nothing,
        _disabled = Nothing,
        _data_ = Nothing
      }
  | otherwise = Nothing

registeredGenerators :: [Generator]
registeredGenerators =
  [ genForSig
  ]

genForSig :: Generator
genForSig =
    Generator {
      title = "Generate signature comments",
      updateAst = updateAstForSig
    }

updateAstForSig :: LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)
updateAstForSig (L loc (SigD _ typeSig@(TypeSig ann ids (HsWC _ (L _ x))))) =
  traceAst "TypeSig" typeSig `seq` Nothing

toRange :: SrcSpan -> Maybe Range
toRange src
  | (RealSrcSpan s _) <- src,
    range' <- realSrcSpanToRange s =
    Just range'
  | otherwise = Nothing

isIntersectWith :: Range -> SrcSpan -> Bool
isIntersectWith Range {_start, _end} x = isInsideSrcSpan _start x || isInsideSrcSpan _end x
