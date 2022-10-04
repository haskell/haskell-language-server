{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ide.Plugin.Diagrams (descriptor) where

import Prelude hiding (mod)

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.Hashable
import Data.List (intercalate)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.String
import Data.Text (Text)
import Data.Time (UTCTime)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram)
import GHC.Generics (Generic)
import System.IO.Temp

import qualified Data.Map          as Map
import qualified Data.Text         as Text
import qualified Diagrams.Prelude  as D

import Development.IDE.Core.Rules (IdeState)
import Development.IDE.GHC.Compat (HscEnv, Name, ModSummary, DynFlags, ModuleName, GhcMonad)
import Development.IDE.Types.Logger
import Language.LSP.Server (MonadLsp)

import qualified Development.IDE                              as IDE
import qualified Development.IDE.Core.Compile                 as IDE
import qualified Development.IDE.Core.FileStore               as IDE
import qualified Development.IDE.Core.Rules                   as IDE
import qualified Development.IDE.GHC.Compat                   as IDE
import qualified Development.IDE.Import.DependencyInformation as IDE
import qualified Development.IDE.Spans.AtPoint                as IDE
import qualified Development.IDE.GHC.Compat                   as GHC
import qualified Development.IDE.GHC.Util                     as GHC
import qualified Development.IDE.Core.Shake                   as Shake
import qualified Development.IDE.Types.Shake                  as Shake

import qualified Ide.Types             as HLS
import qualified Ide.Plugin.Eval.Rules as Eval

import qualified Language.LSP.Types  as LSP

-- TODO: The GHC.NoCompat imports will not work across multiple ghc versions
import qualified GHC                         as GHC.NoCompat
import qualified GHC.LanguageExtensions.Type as GHC.NoCompat

import Ide.Plugin.Diagrams.CatchErrors

descriptor ::
     Recorder (WithPriority Log)
  -> HLS.PluginId
  -> HLS.PluginDescriptor IdeState
descriptor recorder pluginId = (HLS.defaultPluginDescriptor pluginId) {
      HLS.pluginRules    = rules recorder
    , HLS.pluginHandlers = HLS.mkPluginHandler LSP.STextDocumentHover $
                             handleHover recorder
    }

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data Log =
    LogShake Shake.Log
  | HoverOver [(Name, IDE.Type)]
  | CreatedTempDir FilePath
  | RenderedDiagram FilePath
  | RenderingFailed

instance Pretty Log where
  pretty (LogShake l)         = pretty l
  pretty (HoverOver ids)      = "hovering over" <+> pretty ids
  pretty (CreatedTempDir fp)  = "created temp dir" <+> pretty fp
  pretty (RenderedDiagram fp) = "rendered diagram" <+> pretty fp
  pretty RenderingFailed      = "rendering failed"

instance Pretty Name where
  pretty = fromString . IDE.printWithoutUniques

instance Pretty GHC.Type where
  pretty = fromString . IDE.printWithoutUniques

{-------------------------------------------------------------------------------
  Plugin state

  TODO: Exit termination handler (to remove the temp dir again)
-------------------------------------------------------------------------------}

data DiagramsTempDir = DiagramsTempDir
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)

type instance IDE.RuleResult DiagramsTempDir = FilePath

rules :: Recorder (WithPriority Log) -> IDE.Rules ()
rules recorder = do
    Shake.defineNoFile (cmapWithPrio LogShake recorder) $ \DiagramsTempDir -> do
      liftIO $ do
        systemTempDir <- getCanonicalTemporaryDirectory
        tempDir       <- createTempDirectory systemTempDir "hls-diagrams"
        logWith recorder Info $ CreatedTempDir tempDir
        return tempDir

{-------------------------------------------------------------------------------
  Collect information about the current module
-------------------------------------------------------------------------------}

data ModuleInfo = ModuleInfo {
      moduleUri      :: LSP.Uri
    , moduleUriNorm  :: LSP.NormalizedUri
    , modulePath     :: FilePath
    , modulePathNorm :: LSP.NormalizedFilePath
    , moduleSummary  :: ModSummary
    , moduleName     :: ModuleName
    , moduleText     :: Text
    }

getModuleInfo ::
     MonadLsp c m
  => IdeState -> LSP.Uri -> CatchErrors c m ModuleInfo
getModuleInfo ide uri = do
    pathNorm <- uriToNormalizedFilePath uriNorm
    summary  <- IDE.msrModSummary <$> getModSummary ide pathNorm
    path     <- uriToFilePath uri
    text     <- getVirtualFileText uriNorm
    return ModuleInfo{
        moduleUri      = uri
      , moduleUriNorm  = uriNorm
      , modulePath     = path
      , modulePathNorm = pathNorm
      , moduleSummary  = summary
      , moduleName     = GHC.moduleName (GHC.ms_mod summary)
      , moduleText     = text
      }
  where
    uriNorm :: LSP.NormalizedUri
    uriNorm = LSP.toNormalizedUri uri

{-------------------------------------------------------------------------------
  Handler: hover
-------------------------------------------------------------------------------}

handleHover ::
     Recorder (WithPriority Log)
  -> HLS.PluginMethodHandler IdeState 'LSP.TextDocumentHover
handleHover
      recorder
      ide
      _pluginId
      LSP.HoverParams{
          _textDocument = LSP.TextDocumentIdentifier{_uri}
        , _position
        } = pluginResponse $ do

    moduleInfo@ModuleInfo{..} <- getModuleInfo ide _uri

    -- Figure out what we hovered over
    (IDE.HAR{hieAst, hieKind}, _posMap) <- getHieAst ide modulePathNorm

    case hieKind of
      IDE.HieFresh -> do
        let ids = getIdsAtPoint hieAst _position
        logWith recorder Info $ HoverOver ids
        case ids of
          [(name, typ)] | looksLikeDiagram typ -> do
            tempDir  <- runAction ide $ IDE.useNoFile_ DiagramsTempDir
            tempFile <- liftIO $ emptyTempFile tempDir "diagram.svg"
            success  <- renderDiagram ide moduleInfo tempFile name
            if success then do
              logWith recorder Info (RenderedDiagram tempFile)
              return $ Just LSP.Hover {
                  _contents = LSP.HoverContents $ LSP.MarkupContent {
                      _kind  = LSP.MkMarkdown
                    , _value = Text.unlines [
                            Text.pack $ "![](" ++ tempFile ++ ")"
                          ]
                    }
                , _range  = Nothing -- Which range should be highlighted
                }
            else do
              logWith recorder Info $ RenderingFailed
              return Nothing
          _otherwise ->
            return Nothing
      IDE.HieFromDisk{} ->
        return Nothing
  where
    looksLikeDiagram :: IDE.Type -> Bool
    looksLikeDiagram = ("Diagram" `isPrefixOf`) . IDE.printWithoutUniques

{-------------------------------------------------------------------------------
  Auxiliary: HLS
-------------------------------------------------------------------------------}

getIdsAtPoint :: IDE.HieASTs GHC.Type -> LSP.Position -> [(Name, IDE.Type)]
getIdsAtPoint ast pos =
     concatMap (mapMaybe (uncurry aux) . Map.toList . GHC.nodeIdentifiers) $
       IDE.pointCommand ast pos GHC.nodeInfo
  where
    aux ::
         IDE.Identifier
      -> IDE.IdentifierDetails IDE.Type
      -> Maybe (Name, IDE.Type)
    aux (Left  _mod) _       = Nothing
    aux (Right name) details = (name,) <$> GHC.identType details

enableCodeGen :: MonadIO m => IdeState -> ModuleInfo -> m ()
enableCodeGen ide ModuleInfo{modulePathNorm} = liftIO $ do
    Eval.queueForEvaluation ide modulePathNorm
    IDE.setSomethingModified
      Shake.VFSUnmodified
      ide
      [Shake.toKey IDE.NeedsCompilation modulePathNorm]
      "hls-diagrams-plugin"

{-------------------------------------------------------------------------------
  Auxiliary: GHC
-------------------------------------------------------------------------------}

-- | Unload any modules that are out of date
unloadOutOfDate :: MonadIO m => IdeState -> HscEnv -> CatchErrors c m ()
unloadOutOfDate ide env = runAction ide $ do
    current <- IDE.currentLinkables
    liftIO $ GHC.unload env $ map (uncurry aux) $ GHC.moduleEnvToList current
  where
    aux :: GHC.NoCompat.Module -> UTCTime -> GHC.Linkable
    aux mod time = GHC.LM time mod []

initialiseSessionForEval ::
     MonadIO m
  => IdeState -> ModuleInfo -> CatchErrors c m HscEnv
initialiseSessionForEval ide ModuleInfo{..} = do
    env <- runAction ide $ do
      env  <- IDE.hscEnv <$>
                IDE.use_ IDE.GhcSessionDeps modulePathNorm
      deps <- IDE.reachableModules <$>
                IDE.use_ IDE.GetDependencyInformation modulePathNorm
      addLinkables env <$> IDE.uses_ IDE.GetLinkable deps
    unloadOutOfDate ide env
    evalGhcEnv env $ do
      GHC.setContext [GHC.IIModule moduleName]
      GHC.modifyDynFlags (const $ exts $ GHC.ms_hspp_opts moduleSummary)
      GHC.getSession
  where
    -- Add all dependencies as linkables to the 'HscEnv'
    -- This will cause them to be compiled when we call 'GetLinkable'
    addLinkables :: HscEnv -> [IDE.LinkableResult] -> HscEnv
    addLinkables env linkables =
        IDE.loadModulesHome (map IDE.linkableHomeMod linkables) env

    -- Default language extensions
    exts :: DynFlags -> DynFlags
    exts =
          flip GHC.xopt_set    GHC.NoCompat.ExtendedDefaultRules
        . flip GHC.xopt_unset  GHC.NoCompat.MonomorphismRestriction

-- | Change the ghci rendering function for results
--
-- We will set this to a rendering function specifically for diagrams.
setPrint :: GhcMonad m => String -> m ()
setPrint fn = do
    evalPrint <- head <$> GHC.NoCompat.runDecls fn
    GHC.modifySession $ \hsc -> hsc {
        GHC.hsc_IC = GHC.setInteractivePrintName (GHC.hsc_IC hsc) evalPrint
      }

addImports :: GhcMonad m => [String] -> m ()
addImports modules = do
    importDecls <- mapM GHC.NoCompat.parseImportDecl modules
    context     <- GHC.getContext
    GHC.setContext $ map GHC.IIDecl importDecls ++ context

{-------------------------------------------------------------------------------
  Evaluating the diagram
-------------------------------------------------------------------------------}

renderDiagram ::
     MonadUnliftIO m
  => IdeState -> ModuleInfo -> FilePath -> Name -> CatchErrors c m Bool
renderDiagram ide moduleInfo tempFile name = do
    enableCodeGen ide moduleInfo
    env    <- initialiseSessionForEval ide moduleInfo
    result <- evalGhcEnv env $ do
                addImports imports
                setPrint renderFunction
                GHC.NoCompat.execStmt' stmt (IDE.printName name) opts
    return $ case result of
               GHC.NoCompat.ExecComplete{execResult = Right _} ->
                 True
               _otherwise ->
                 False
  where
    stmt :: GHC.GhciLStmt GHC.GhcPs
    stmt = GHC.noLoc $ GHC.BodyStmt
             GHC.noExtField
             (GHC.noLoc $ GHC.HsVar GHC.noExtField $ GHC.noLoc $ GHC.Exact name)
             GHC.noSyntaxExpr
             GHC.noSyntaxExpr

    -- TODO: Set filename and line number etc
    opts :: GHC.NoCompat.ExecOptions
    opts = GHC.NoCompat.execOptions

    imports :: [String]
    imports = [
          "import qualified Diagrams.Backend.SVG"
        , "import qualified Diagrams.TwoD.Size"
        ]

    renderFunction :: String
    renderFunction =
        intercalate " " [
            "evalPrint x ="
          , "Diagrams.Backend.SVG.renderSVG"
          , show tempFile
          , "(Diagrams.TwoD.Size.dims2D 100 100)"
          , "x"
          ]

{-------------------------------------------------------------------------------
  Example diagram
-------------------------------------------------------------------------------}

_myCircle :: Diagram B
_myCircle = D.circle 1
