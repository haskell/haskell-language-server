{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Ide.Plugin.Class.CodeLens where

import           Control.Lens                    ((^.))
import           Control.Monad.IO.Class          (liftIO)
import           Data.Aeson
import           Data.Maybe                      (mapMaybe, maybeToList)
import qualified Data.Text                       as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           GHC.LanguageExtensions.Type
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server             (sendRequest)
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens         as J

codeLens :: PluginMethodHandler IdeState TextDocumentCodeLens
codeLens state plId CodeLensParams{..} = do
    enabled <- enableTypeLens <$> getCompletionsConfig plId
    if not enabled then pure $ pure $ List [] else pluginResponse $ do
        nfp <- getNormalizedFilePath plId uri
        tmr <- handleMaybeM "Unable to typecheak"
            $ liftIO
            $ runAction "classplugin.TypeCheck" state
            $ use TypeCheck nfp

        -- All instance binds
        InstanceBindTypeSigsResult allBinds <-
            handleMaybeM "Unable to get InstanceBindTypeSigsResult"
            $ liftIO
            $ runAction "classplugin.GetInstanceBindTypeSigs" state
            $ use GetInstanceBindTypeSigs nfp

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
                    $ makeEdit range title
            codeLens = makeLens <$> mapMaybe getRangeWithSig targetSigs

        pure $ List codeLens
    where
        uri = _textDocument ^. J.uri

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
                    FunBind{..} -> Just $ L l fun_id
                    _           -> Nothing
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
                (pure [(uri, List $ edits ++ pragmaInsertion)])
                Nothing
                Nothing

        generateLens :: PluginId -> Range -> T.Text -> WorkspaceEdit -> CodeLens
        generateLens plId range title edit =
            let cmd = mkLspCommand plId typeLensCommandId title (Just [toJSON edit])
            in  CodeLens range (Just cmd) Nothing

        makeEdit :: Range -> T.Text -> [TextEdit]
        makeEdit range bind =
            let startPos = range ^. J.start
                insertChar = startPos ^. J.character
                insertRange = Range startPos startPos
            in [TextEdit insertRange (bind <> "\n" <> T.replicate (fromIntegral insertChar) " ")]

codeLensCommandHandler :: CommandFunction IdeState WorkspaceEdit
codeLensCommandHandler _ wedit = do
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right Null
