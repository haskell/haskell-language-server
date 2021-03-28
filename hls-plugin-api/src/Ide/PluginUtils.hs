{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
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
    getPid,
    allLspCmdIds,
    allLspCmdIds',
    installSigUsr1Handler,
    subRange,
    usePropertyLsp)
where


import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.HashMap.Strict             as H
import qualified Data.Text                       as T
import           Ide.Types
import           Language.LSP.Types
import qualified Language.LSP.Types              as J
import           Language.LSP.Types.Capabilities

import qualified Data.Map.Strict                 as Map
import           Ide.Plugin.Config
import           Ide.Plugin.Properties
import           Language.LSP.Server

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
    then WorkspaceEdit Nothing (Just docChanges) Nothing
    else WorkspaceEdit (Just h) Nothing Nothing
  where
    diff = diffTextEdit fText f2Text withDeletions
    h = H.singleton f diff
    docChanges = J.List [InL docEdit]
    docEdit = J.TextDocumentEdit (J.VersionedTextDocumentIdentifier f (Just 0)) $ fmap InL diff

-- ---------------------------------------------------------------------

clientSupportsDocumentChanges :: ClientCapabilities -> Bool
clientSupportsDocumentChanges caps =
  let ClientCapabilities mwCaps _ _ _ = caps
      supports = do
        wCaps <- mwCaps
        WorkspaceEditClientCapabilities mDc _ _ _ _ <- _workspaceEdit wCaps
        mDc
  in
    Just True == supports

-- ---------------------------------------------------------------------

pluginDescToIdePlugins :: [PluginDescriptor ideState] -> IdePlugins ideState
pluginDescToIdePlugins plugins = IdePlugins $ Map.fromList $ map (\p -> (pluginId p, p)) plugins


-- ---------------------------------------------------------------------
-- | Returns the current client configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can at runitime change
-- their configuration.
--
getClientConfig :: MonadLsp Config m => m Config
getClientConfig = getConfig

-- ---------------------------------------------------------------------

-- | Returns the current plugin configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can change their
-- configuration at runtime.
getPluginConfig :: MonadLsp Config m => PluginId -> m PluginConfig
getPluginConfig plugin = do
    config <- getClientConfig
    return $ flip configForPlugin plugin config

-- ---------------------------------------------------------------------

-- | Returns the value of a property defined by the current plugin.
usePropertyLsp ::
  (HasProperty s k t r, MonadLsp Config m) =>
  KeyNameProxy s ->
  PluginId ->
  Properties r ->
  m (ToHsType t)
usePropertyLsp kn pId p = do
  config <- getPluginConfig pId
  return $ useProperty kn p $ plcConfig config

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

subRange :: Range -> Range -> Bool
subRange smallRange range =
     positionInRange (_start smallRange) range
  && positionInRange (_end smallRange) range

positionInRange :: Position -> Range -> Bool
positionInRange (Position pl po) (Range (Position sl so) (Position el eo)) =
     pl >  sl && pl <  el
  || pl == sl && pl == el && po >= so && po <= eo
  || pl == sl && po >= so
  || pl == el && po <= eo

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
allLspCmdIds pid commands = concatMap go commands
  where
    go (plid, cmds) = map (mkLspCmdId pid plid . commandId) cmds

