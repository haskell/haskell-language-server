{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.PluginUtils
  ( WithDeletions(..),
    getProcessID,
    normalize,
    makeDiffTextEdit,
    makeDiffTextEditAdditive,
    diffText,
    diffText',
    pluginDescToIdePlugins,
    responseError,
    getClientConfig,
    getPluginConfig,
    configForPlugin,
    pluginEnabled,
    extractRange,
    fullRange,
    mkLspCommand,
    mkLspCmdId,
  allLspCmdIds,allLspCmdIds',installSigUsr1Handler)
where


import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.HashMap.Strict                     as H
import           Data.Maybe
import qualified Data.Text                               as T
import           Ide.Types
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types              as J
import           Language.Haskell.LSP.Types.Capabilities

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process                    as P (getCurrentProcessId)
#else
import           System.Posix.Signals
import qualified System.Posix.Process                    as P (getProcessID)
#endif
import qualified Data.Aeson                              as J
import qualified Data.Default
import qualified Data.Map.Strict                         as Map
import           Ide.Plugin.Config
import qualified Language.Haskell.LSP.Core               as LSP
import Control.Monad (void)

-- ---------------------------------------------------------------------

-- | Extend to the line below and above to replace newline character.
normalize :: Range -> Range
normalize (Range (Position sl _) (Position el _)) =
  Range (Position sl 0) (Position (el + 1) 0)

-- ---------------------------------------------------------------------

data WithDeletions = IncludeDeletions | SkipDeletions
  deriving Eq

-- | Generate a 'WorkspaceEdit' value from a pair of source Text
diffText :: ClientCapabilities -> (Uri,T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText clientCaps old new withDeletions =
  let
    supports = clientSupportsDocumentChanges clientCaps
  in diffText' supports old new withDeletions

makeDiffTextEdit :: T.Text -> T.Text -> List TextEdit
makeDiffTextEdit f1 f2 = diffTextEdit f1 f2 IncludeDeletions

makeDiffTextEditAdditive :: T.Text -> T.Text -> List TextEdit
makeDiffTextEditAdditive f1 f2 = diffTextEdit f1 f2 SkipDeletions

diffTextEdit :: T.Text -> T.Text -> WithDeletions -> List TextEdit
diffTextEdit fText f2Text withDeletions = J.List r
  where
    r = map diffOperationToTextEdit diffOps
    d = getGroupedDiff (lines $ T.unpack fText) (lines $ T.unpack f2Text)

    diffOps = filter (\x -> (withDeletions == IncludeDeletions) || not (isDeletion x))
                     (diffToLineRanges d)

    isDeletion (Deletion _ _) = True
    isDeletion _              = False


    diffOperationToTextEdit :: DiffOperation LineRange -> J.TextEdit
    diffOperationToTextEdit (Change fm to) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = T.pack $ init $ unlines $ lrContents to

    {-
      In order to replace everything including newline characters,
      the end range should extend below the last line. From the specification:
      "If you want to specify a range that contains a line including
      the line ending character(s) then use an end position denoting
      the start of the next line"
    -}
    diffOperationToTextEdit (Deletion (LineRange (sl, el) _) _) = J.TextEdit range ""
      where
        range = J.Range (J.Position (sl - 1) 0)
                        (J.Position el 0)

    diffOperationToTextEdit (Addition fm l) = J.TextEdit range nt
    -- fm has a range wrt to the changed file, which starts in the current file at l + 1
    -- So the range has to be shifted to start at l + 1
      where
        range = J.Range (J.Position l 0)
                        (J.Position l 0)
        nt = T.pack $ unlines $ lrContents fm


    calcRange fm = J.Range s e
      where
        sl = fst $ lrNumbers fm
        sc = 0
        s = J.Position (sl - 1) sc -- Note: zero-based lines
        el = snd $ lrNumbers fm
        ec = length $ last $ lrContents fm
        e = J.Position (el - 1) ec  -- Note: zero-based lines


-- | A pure version of 'diffText' for testing
diffText' :: Bool -> (Uri,T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText' supports (f,fText) f2Text withDeletions  =
  if supports
    then WorkspaceEdit Nothing (Just docChanges)
    else WorkspaceEdit (Just h) Nothing
  where
    diff = diffTextEdit fText f2Text withDeletions
    h = H.singleton f diff
    docChanges = J.List [docEdit]
    docEdit = J.TextDocumentEdit (J.VersionedTextDocumentIdentifier f (Just 0)) diff

-- ---------------------------------------------------------------------

clientSupportsDocumentChanges :: ClientCapabilities -> Bool
clientSupportsDocumentChanges caps =
  let ClientCapabilities mwCaps _ _ _ = caps
      supports = do
        wCaps <- mwCaps
        WorkspaceEditClientCapabilities mDc <- _workspaceEdit wCaps
        mDc
  in
    fromMaybe False supports

-- ---------------------------------------------------------------------

pluginDescToIdePlugins :: [PluginDescriptor ideState] -> IdePlugins ideState
pluginDescToIdePlugins plugins = IdePlugins $ Map.fromList $ map (\p -> (pluginId p, p)) plugins


-- ---------------------------------------------------------------------

responseError :: T.Text -> ResponseError
responseError txt = ResponseError InvalidParams txt Nothing


-- ---------------------------------------------------------------------
-- | Returns the current client configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can at runitime change
-- their configuration.
--
-- If no custom configuration has been set by the client, this function returns
-- our own defaults.
getClientConfig :: LSP.LspFuncs Config -> IO Config
getClientConfig lf = fromMaybe Data.Default.def <$> LSP.config lf

-- ---------------------------------------------------------------------

-- | Returns the current plugin configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can change their
-- configuration at runtime.
--
-- If no custom configuration has been set by the client, this function returns
-- our own defaults.
getPluginConfig :: LSP.LspFuncs Config -> PluginId -> IO PluginConfig
getPluginConfig lf plugin = do
    config <- getClientConfig lf
    return $ configForPlugin config plugin

configForPlugin :: Config -> PluginId -> PluginConfig
configForPlugin config (PluginId plugin)
    = Map.findWithDefault Data.Default.def plugin (plugins config)

-- ---------------------------------------------------------------------

-- | Checks that a given plugin is both enabled and the specific feature is
-- enabled
pluginEnabled :: PluginConfig -> (PluginConfig -> Bool) -> Bool
pluginEnabled pluginConfig f = plcGlobalOn pluginConfig && f pluginConfig

-- ---------------------------------------------------------------------

extractRange :: Range -> T.Text -> T.Text
extractRange (Range (Position sl _) (Position el _)) s = newS
  where focusLines = take (el-sl+1) $ drop sl $ T.lines s
        newS = T.unlines focusLines

-- | Gets the range that covers the entire text
fullRange :: T.Text -> Range
fullRange s = Range startPos endPos
  where startPos = Position 0 0
        endPos = Position lastLine 0
        {-
        In order to replace everything including newline characters,
        the end range should extend below the last line. From the specification:
        "If you want to specify a range that contains a line including
        the line ending character(s) then use an end position denoting
        the start of the next line"
        -}
        lastLine = length $ T.lines s

-- ---------------------------------------------------------------------

allLspCmdIds' :: T.Text -> IdePlugins ideState -> [T.Text]
allLspCmdIds' pid mp = mkPlugin (allLspCmdIds pid) (Just . pluginCommands)
    where
        justs (p, Just x)  = [(p, x)]
        justs (_, Nothing) = []

        ls = Map.toList (ipMap mp)

        mkPlugin maker selector
            = maker $ concatMap (\(pid, p) -> justs (pid, selector p)) ls


allLspCmdIds :: T.Text -> [(PluginId, [PluginCommand ideState])] -> [T.Text]
allLspCmdIds pid commands = concat $ map go commands
  where
    go (plid, cmds) = map (mkLspCmdId pid plid . commandId) cmds

mkLspCommand :: PluginId -> CommandId -> T.Text -> Maybe [J.Value] -> IO Command
mkLspCommand plid cn title args' = do
  pid <- getPid
  let cmdId = mkLspCmdId pid plid cn
  let args = List <$> args'
  return $ Command title cmdId args

mkLspCmdId :: T.Text -> PluginId -> CommandId -> T.Text
mkLspCmdId pid (PluginId plid) (CommandId cid)
  = pid <> ":" <> plid <> ":" <> cid

-- | Get the operating system process id for the running server
-- instance. This should be the same for the lifetime of the instance,
-- and different from that of any other currently running instance.
getPid :: IO T.Text
getPid = T.pack . show <$> getProcessID

getProcessID :: IO Int
installSigUsr1Handler :: IO () -> IO ()

#ifdef mingw32_HOST_OS
getProcessID = fromIntegral <$> P.getCurrentProcessId
installSigUsr1Handler _ = return ()

#else
getProcessID = fromIntegral <$> P.getProcessID

installSigUsr1Handler h = void $ installHandler sigUSR1 (Catch h) Nothing
#endif
