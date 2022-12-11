{-# LANGUAGE CPP          #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Ide.Plugin.Brittany where

import           Control.Exception                               (bracket_)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe                       (MaybeT,
                                                                  runMaybeT)
import           Data.Maybe                                      (fromMaybe,
                                                                  mapMaybe,
                                                                  maybeToList)
import           Data.Semigroup
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import           Development.IDE                                 hiding
                                                                 (getExtensions,
                                                                  pluginHandlers)
import qualified Development.IDE.GHC.Compat                      as GHC hiding
                                                                        (Cpp)
import qualified Development.IDE.GHC.Compat.Util                 as GHC
import           GHC.LanguageExtensions.Type
import           Ide.PluginUtils
import           Ide.Types                                       hiding (Config)
import           Language.Haskell.Brittany
import           Language.LSP.Types                              as J
import qualified Language.LSP.Types.Lens                         as J
import           System.Environment                              (setEnv,
                                                                  unsetEnv)
import           System.FilePath

-- These imports are for the temporary pPrintText & can be removed when
-- issue #2005 is resolved
import           Control.Monad.Trans.Class                       (lift)
import qualified Control.Monad.Trans.Except                      as ExceptT
import           Data.CZipWith
import qualified Data.List                                       as List
import qualified Data.Text                                       as Text
import qualified Data.Text.Lazy                                  as TextL
import qualified GHC.LanguageExtensions.Type                     as GHC
import           Language.Haskell.Brittany.Internal
import           Language.Haskell.Brittany.Internal.Config
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Obfuscation
import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.Utils
import qualified Language.Haskell.GHC.ExactPrint                 as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types           as ExactPrint


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkFormattingHandlers provider
  }

-- | Formatter provider of Brittany.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingHandler IdeState
provider ide typ contents nfp opts = liftIO $ do
    confFile <- getConfFile nfp
    let (range, selectedContents) = case typ of
          FormatText    -> (fullRange contents, contents)
          FormatRange r -> (normalize r, extractRange r contents)
    modsum <- fmap msrModSummary $ runAction "brittany" ide $ use_ GetModSummaryWithoutTimestamps nfp
    let dflags = GHC.ms_hspp_opts modsum
    let withRuntimeLibdir = bracket_ (setEnv key $ GHC.topDir dflags) (unsetEnv key)
          where key = "GHC_EXACTPRINT_GHC_LIBDIR"
    res <- withRuntimeLibdir $ formatText dflags confFile opts selectedContents
    case res of
      Left err -> return $ Left $ responseError (T.pack $ "brittanyCmd: " ++ unlines (map showErr err))
      Right newText -> return $ Right $ J.List [TextEdit range newText]

-- | Primitive to format text with the given option.
-- May not throw exceptions but return a Left value.
-- Errors may be presented to the user.
formatText
  :: MonadIO m
  => GHC.DynFlags
  -> Maybe FilePath -- ^ Path to configs. If Nothing, default configs will be used.
  -> FormattingOptions -- ^ Options for the formatter such as indentation.
  -> Text -- ^ Text to format
  -> m (Either [BrittanyError] Text) -- ^ Either formatted Text or a error from Brittany.
formatText df confFile opts text =
  liftIO $ runBrittany tabSize df confFile text
  where tabSize = fromIntegral $ opts ^. J.tabSize

-- | Recursively search in every directory of the given filepath for brittany.yaml.
-- If no such file has been found, return Nothing.
getConfFile :: NormalizedFilePath -> IO (Maybe FilePath)
getConfFile = findLocalConfigPath . takeDirectory . fromNormalizedFilePath

-- | Run Brittany on the given text with the given tab size and
-- a configuration path. If no configuration path is given, a
-- default configuration is chosen. The configuration may overwrite
-- tab size parameter.
--
-- Returns either a list of Brittany Errors or the reformatted text.
-- May not throw an exception.
runBrittany :: Int              -- ^ tab  size
            -> GHC.DynFlags
            -> Maybe FilePath   -- ^ local config file
            -> Text             -- ^ text to format
            -> IO (Either [BrittanyError] Text)
runBrittany tabSize df confPath text = do
  let cfg = mempty
              { _conf_layout =
                  mempty { _lconfig_indentAmount = opt (Last tabSize)
                         }
              , _conf_forward =
                  (mempty :: CForwardOptions CMaybe)
                    { _options_ghc = opt (getExtensions df)
                    }
              }
  config <- fromMaybeT (pure staticDefaultConfig)
                       (readConfigsWithUserConfig cfg (maybeToList confPath))
  (errsAndWarnings, resultText) <- pPrintText config text
  if any isError errsAndWarnings then
    return $ Left errsAndWarnings
  else
    return $ Right resultText

#if MIN_VERSION_brittany(0,14,0)
type CMaybe = Maybe
opt :: a -> Maybe a
opt = Just
#else
type CMaybe = Option
opt :: a -> Option a
opt = Option . Just
#endif

fromMaybeT :: Monad m => m a -> MaybeT m a -> m a
fromMaybeT def act = runMaybeT act >>= maybe def return

showErr :: BrittanyError -> String
showErr (ErrorInput s)          = s
showErr (ErrorMacroConfig  err input)
  = "Error: parse error in inline configuration: " ++ err ++ " in the string \"" ++ input ++ "\"."
showErr (ErrorUnusedComment s)  = s
showErr (LayoutWarning s)       = s
showErr (ErrorUnknownNode s _)  = s
showErr ErrorOutputCheck        = "Brittany error - invalid output"

showExtension :: Extension -> Maybe String
showExtension Cpp              = Just "-XCPP"
-- Brittany chokes on parsing extensions that produce warnings
showExtension DatatypeContexts = Nothing
showExtension RecordPuns       = Just "-XNamedFieldPuns"
showExtension other            = Just $ "-X" ++ show other

getExtensions :: GHC.DynFlags -> [String]
getExtensions = mapMaybe showExtension . GHC.toList . GHC.extensionFlags


-- | This is a temporary fix that allows us to format the text if brittany
-- throws warnings during pretty printing.
--
-- It should be removed when our PR to brittany is merged + released.
-- See:
--  - https://github.com/haskell/haskell-language-server/issues/2005
--  - https://github.com/lspitzner/brittany/pull/351
pPrintText
  :: Config -- ^ global program config
  -> Text   -- ^ input text
  -> IO ([BrittanyError], Text) -- ^ list of errors/warnings & result text
pPrintText config text =
  fmap (either id id) . ExceptT.runExceptT $ do
    let ghcOptions = config & _conf_forward & _options_ghc & runIdentity
    -- there is a good of code duplication between the following code and the
    -- `pureModuleTransform` function. Unfortunately, there are also a good
    -- amount of slight differences: This module is a bit more verbose, and
    -- it tries to use the full-blown `parseModule` function which supports
    -- CPP (but requires the input to be a file..).
    let cppMode    = config & _conf_preprocessor & _ppconf_CPPMode & confUnpack
    -- the flag will do the following: insert a marker string
    -- ("-- BRITANY_INCLUDE_HACK ") right before any lines starting with
    -- "#include" before processing (parsing) input; and remove that marker
    -- string from the transformation output.
    -- The flag is intentionally misspelled to prevent clashing with
    -- inline-config stuff.
    let hackAroundIncludes =
          config & _conf_preprocessor & _ppconf_hackAroundIncludes & confUnpack
    let exactprintOnly = viaGlobal || viaDebug
         where
          viaGlobal = config & _conf_roundtrip_exactprint_only & confUnpack
          viaDebug =
            config & _conf_debug & _dconf_roundtrip_exactprint_only & confUnpack

    let cppCheckFunc dynFlags = if GHC.xopt GHC.Cpp dynFlags
          then case cppMode of
            CPPModeAbort ->
              return $ Left "Encountered -XCPP. Aborting."
            CPPModeWarn ->
              return $ Right True
            CPPModeNowarn ->
              return $ Right True
          else return $ Right False
    parseResult <- do
        -- TODO: refactor this hack to not be mixed into parsing logic
        let hackF s = if "#include" `List.isPrefixOf` s
              then "-- BRITANY_INCLUDE_HACK " ++ s
              else s
        let hackTransform = if hackAroundIncludes && not exactprintOnly
              then List.intercalate "\n" . fmap hackF . lines'
              else id
        liftIO $ parseModuleFromString ghcOptions
                                                   "stdin"
                                                   cppCheckFunc
                                                   (hackTransform $ Text.unpack text)
    case parseResult of
      Left left -> do
        ExceptT.throwE ([ErrorInput left], text)
      Right (anns, parsedSource, hasCPP) -> do
        (inlineConf, perItemConf) <-
          case
            extractCommentConfigs anns (getTopLevelDeclNameMap parsedSource)
          of
            Left (err, input) -> do
              let errMsg =
                    "Error: parse error in inline configuration: "
                    <> err
                    <> "  in the string \""
                    <> input
                    <> "\"."
              ExceptT.throwE ([ErrorInput errMsg], text)
            Right c ->
              pure c
        let moduleConf = cZipWith fromOptionIdentity config inlineConf
        let disableFormatting =
              moduleConf & _conf_disable_formatting & confUnpack
        (errsWarns, outSText, _) <- do
          if
            | disableFormatting -> do
              pure ([], text, False)
            | exactprintOnly -> do
              let r = Text.pack $ ExactPrint.exactPrint parsedSource anns
              pure ([], r, r /= text)
            | otherwise -> do
              (ews, outRaw) <- if hasCPP
                then return
                  $ pPrintModule moduleConf perItemConf anns parsedSource
                else liftIO $ pPrintModuleAndCheck moduleConf
                                                   perItemConf
                                                   anns
                                                   parsedSource
              let hackF s = fromMaybe s $ TextL.stripPrefix
                    (TextL.pack "-- BRITANY_INCLUDE_HACK ")
                    s
              let out = TextL.toStrict $ if hackAroundIncludes
                    then
                      TextL.intercalate (TextL.pack "\n")
                      $ hackF
                      <$> TextL.splitOn (TextL.pack "\n") outRaw
                    else outRaw
              out' <- if moduleConf & _conf_obfuscate & confUnpack
                then lift $ obfuscate out
                else pure out
              pure (ews, out', out' /= text)
        let customErrOrder ErrorInput{}         = 4
            customErrOrder LayoutWarning{}      = -1 :: Int
            customErrOrder ErrorOutputCheck{}   = 1
            customErrOrder ErrorUnusedComment{} = 2
            customErrOrder ErrorUnknownNode{}   = -2 :: Int
            customErrOrder ErrorMacroConfig{}   = 5
            hasErrors =
              if config & _conf_errorHandling & _econf_Werror & confUnpack
                then not $ null errsWarns
                else 0 < maximum (-1 : fmap customErrOrder errsWarns)
        return (errsWarns, if hasErrors then text else outSText)

isError :: BrittanyError -> Bool
isError = \case
    LayoutWarning{}    -> False
    ErrorUnknownNode{} -> False
    _                  -> True
