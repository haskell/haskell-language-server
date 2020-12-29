{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Splice
    ( descriptor,
    )
where

import Control.Lens hiding (List, use)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS
import Data.Aeson
import qualified Data.DList as DL
import Data.Monoid (Ap (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Development.IDE
import Development.IDE.GHC.Compat hiding (getLoc)
import GhcPlugins hiding (Var, getLoc, (<>))
import Ide.Logger (debugm)
import Ide.Plugin
import Ide.Plugin.Splice.Types
import Ide.Types
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as J

descriptor :: PluginId -> PluginDescriptor
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginCommands = commands
        , pluginCodeActionProvider = Just codeAction
        }

--
expandInplaceId, expandCommentedId :: CommandId
expandInplaceId = "expandTHSpliceInplace"
expandCommentedId = "expandTHSpliceCommented"

inplaceCmdName :: T.Text
inplaceCmdName = "expand TemplateHaskell Splice (in-place)"

commentedCmdName :: T.Text
commentedCmdName = "expand TemplateHaskell Splice (comented-out)"

commands :: [PluginCommand]
commands =
    [ PluginCommand expandInplaceId inplaceCmdName $ expandTHSplice Inplace
    , PluginCommand expandCommentedId commentedCmdName $ expandTHSplice Commented
    ]

expandTHSplice ::
    -- | Inplace?
    ExpandStyle ->
    CommandFunction ExpandSpliceParams
expandTHSplice _eStyle lsp _ params@ExpandSpliceParams {..} = do
    sendFunc lsp $
        NotShowMessage $
            NotificationMessage "2.0" WindowShowMessage $
                ShowMessageParams MtInfo $
                    T.unlines
                        [ "## Expanding splice for: "
                        , "-" <> T.pack (show params)
                        , "(lie)"
                        ]
    pure (Right Null, Nothing)

codeAction :: CodeActionProvider
codeAction _ state plId docId _ _ =
    fmap (maybe (Right $ List []) Right) $
        runMaybeT $ do
            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri theUri
            (getAsts . hieAst -> asts, _) <-
                MaybeT . runAction "splice" state $
                    useWithStale GetHieAst fp
            fmap (List . DL.toList) $ execWriterT $ alaf Ap foldMap go asts
    where
        theUri = docId ^. J.uri
        go ast = forM (flattenAst ast) $ \Node {..} -> do
            let NodeInfo {..} = nodeInfo
                spCxt
                    | ("SplicePat", "Pat") `Set.member` nodeAnnotations =
                        Just Pat
                    | ("HsSpliceE", "HsExpr") `Set.member` nodeAnnotations = Just Expr
                    | ("HsSpliceTy", "HsType") `Set.member` nodeAnnotations = Just HsType
                    | ("SpliceD", "HsDecl") `Set.member` nodeAnnotations = Just HsDecl
                    | otherwise = Nothing
            forM_ spCxt $ \spliceContext -> forM_ expandStyles $ \(_style, title, cmdId) ->
                let range = realSrcSpanToRange nodeSpan
                    params = ExpandSpliceParams {uri = theUri, ..}
                 in CACodeAction
                        . CodeAction title (Just CodeActionRefactorRewrite) Nothing Nothing
                        . Just
                        <$> liftIO (mkLspCommand plId cmdId title (Just [toJSON params]))

expandStyles :: [(ExpandStyle, T.Text, CommandId)]
expandStyles =
    [ (Inplace, inplaceCmdName, expandInplaceId)
    , (Commented, commentedCmdName, expandCommentedId)
    ]
