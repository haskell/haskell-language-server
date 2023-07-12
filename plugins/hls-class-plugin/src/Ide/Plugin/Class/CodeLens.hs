{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Ide.Plugin.Class.CodeLens where

import           Control.Lens                         ((^.))
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           hiding (Null)
import           Data.Maybe                           (mapMaybe, maybeToList)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           GHC.LanguageExtensions.Type
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server                  (sendRequest)

codeLens :: PluginMethodHandler IdeState Method_TextDocumentCodeLens
codeLens state plId CodeLensParams{..} = pluginResponse $ do
    nfp <- getNormalizedFilePath uri
    (tmr, _) <- handleMaybeM "Unable to typecheck"
        $ liftIO
        $ runAction "classplugin.TypeCheck" state
        -- Using stale results means that we can almost always return a value. In practice
        -- this means the lenses don't 'flicker'
        $ useWithStale TypeCheck nfp

    -- All instance binds
    (InstanceBindTypeSigsResult allBinds, mp) <-
        handleMaybeM "Unable to get InstanceBindTypeSigsResult"
        $ liftIO
        $ runAction "classplugin.GetInstanceBindTypeSigs" state
        -- Using stale results means that we can almost always return a value. In practice
        -- this means the lenses don't 'flicker'
        $ useWithStale GetInstanceBindTypeSigs nfp

    pragmaInsertion <- insertPragmaIfNotPresent state nfp InstanceSigs

    let (hsGroup, _, _, _) = tmrRenamed tmr
        tycls = hs_tyclds hsGroup
        -- declared instance methods without signatures
        bindInfos = [ bind
                    | instds <- map group_instds tycls -- class instance decls
                    , instd <- instds
                    , inst <- maybeToList $ getClsInstD (unLoc instd)
                    , bind <- getBindSpanWithoutSig inst
                    ]
        targetSigs = matchBind bindInfos allBinds
        makeLens (range, title) =
            generateLens plId range title
                $ workspaceEdit pragmaInsertion
                $ makeEdit range title mp
        codeLens = makeLens <$> mapMaybe getRangeWithSig targetSigs

    pure $ InL codeLens
    where
        uri = _textDocument ^. L.uri

        -- Match Binds with their signatures
        -- We try to give every `InstanceBindTypeSig` a `SrcSpan`,
        -- hence we can display signatures for `InstanceBindTypeSig` with span later.
        matchBind :: [BindInfo] -> [InstanceBindTypeSig] -> [InstanceBindTypeSig]
        matchBind existedBinds allBindWithSigs =
            [foldl go bindSig existedBinds | bindSig <- allBindWithSigs]
            where
                -- | The `bindDefSpan` of the bind is `Nothing` before,
                -- we update it with the span where binding occurs.
                -- Hence, we can infer the place to display the signature later.
                update :: InstanceBindTypeSig -> SrcSpan -> InstanceBindTypeSig
                update bind sp = bind {bindDefSpan = Just sp}

                go :: InstanceBindTypeSig -> BindInfo -> InstanceBindTypeSig
                go bindSig bind = case (srcSpanToRange . bindNameSpan) bind of
                    Nothing -> bindSig
                    Just range ->
                        if inRange range (getSrcSpan $ bindName bindSig)
                            then update bindSig (bindSpan bind)
                            else bindSig

        getClsInstD (ClsInstD _ d) = Just d
        getClsInstD _              = Nothing

        getSigName (ClassOpSig _ _ sigNames _) = Just $ map unLoc sigNames
        getSigName _                           = Nothing

        getBindSpanWithoutSig :: ClsInstDecl GhcRn -> [BindInfo]
        getBindSpanWithoutSig ClsInstDecl{..} =
            let bindNames = mapMaybe go (bagToList cid_binds)
                go (L l bind) = case bind of
                    FunBind{..}
                        -- `Generated` tagged for Template Haskell,
                        -- here we filter out nonsence generated bindings
                        -- that are nonsense for displaying code lenses.
                        --
                        -- See https://github.com/haskell/haskell-language-server/issues/3319
                        | not $ isGenerated (groupOrigin fun_matches)
                            -> Just $ L l fun_id
                    _       -> Nothing
                -- Existed signatures' name
                sigNames = concat $ mapMaybe (\(L _ r) -> getSigName r) cid_sigs
                toBindInfo (L l (L l' _)) = BindInfo
                    (locA l) -- bindSpan
                    (locA l') -- bindNameSpan
            in toBindInfo <$> filter (\(L _ name) -> unLoc name `notElem` sigNames) bindNames
        getBindSpanWithoutSig _ = []

        -- Get bind definition range with its rendered signature text
        getRangeWithSig :: InstanceBindTypeSig -> Maybe (Range, T.Text)
        getRangeWithSig bind = do
            span <- bindDefSpan bind
            range <- srcSpanToRange span
            pure (range, bindRendered bind)

        workspaceEdit pragmaInsertion edits =
            WorkspaceEdit
                (pure [(uri, edits ++ pragmaInsertion)])
                Nothing
                Nothing

        generateLens :: PluginId -> Range -> T.Text -> WorkspaceEdit -> CodeLens
        generateLens plId range title edit =
            let cmd = mkLspCommand plId typeLensCommandId title (Just [toJSON edit])
            in  CodeLens range (Just cmd) Nothing

        makeEdit :: Range -> T.Text -> PositionMapping -> [TextEdit]
        makeEdit range bind mp =
            let startPos = range ^. L.start
                insertChar = startPos ^. L.character
                insertRange = Range startPos startPos
            in case toCurrentRange mp insertRange of
                Just rg -> [TextEdit rg (bind <> "\n" <> T.replicate (fromIntegral insertChar) " ")]
                Nothing -> []

codeLensCommandHandler :: CommandFunction IdeState WorkspaceEdit
codeLensCommandHandler _ wedit = do
  _ <- sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right $ InR Null
