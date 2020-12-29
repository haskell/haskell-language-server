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
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Development.IDE
import Development.IDE.Core.PositionMapping
import Development.IDE.GHC.Compat hiding (getLoc)
import GhcPlugins hiding (Var, getLoc, (<>))
import Ide.Logger (debugm)
import Ide.Plugin
import Ide.Plugin.Splice.Types
    ( ExpandSpliceParams (..),
      ExpandStyle (..),
      SpliceContext (Expr, HsDecl, HsType, Pat),
    )
import Ide.Types
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Messages
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
                        , "-" <> T.pack (show (_eStyle, params))
                        , "(lie)"
                        ]
    pure (Right Null, Nothing)

-- TODO: workaround when HieAst unavailable (e.g. when the module itself errors)
-- TODO: Declaration Splices won't appear in HieAst; perhaps we must just use Parsed/Renamed ASTs?
codeAction :: CodeActionProvider
codeAction lsp state plId docId range0 _ =
    fmap (maybe (Right $ List []) Right) $
        runMaybeT $ do
            fp <- MaybeT $ pure $ uriToNormalizedFilePath $ toNormalizedUri theUri
            (getAsts . hieAst -> asts, posMap) <-
                MaybeT . runAction "splice" state $
                    useWithStale GetHieAst fp
            ran' <-
                MaybeT $
                    pure $
                        fromCurrentRange posMap range0
            fmap (List . DL.toList) $
                execWriterT $
                    getAp $ ifoldMap (fmap Ap . go ran') asts
    where
        sloc pos fs = mkRealSrcLoc fs (line pos + 1) (cha pos + 1)
        sp ran fs = mkRealSrcSpan (sloc (_start ran) fs) (sloc (_end ran) fs)
        line ran = _line ran
        cha ran = _character ran

        theUri = docId ^. J.uri
        go ran' fs ast = do
            trace ("astanns: " <> show (foldMap (DL.singleton . nodeAnnotations . nodeInfo) (flattenAst ast))) $ pure ()
            forM_ (smallestContainingSatisfying (sp ran' fs) isSpliceNode ast) $
                \Node {..} -> do
                    let NodeInfo {..} = nodeInfo
                        spCxt
                            | ("SplicePat", "Pat") `Set.member` nodeAnnotations =
                                Just Pat
                            | ("HsSpliceE", "HsExpr") `Set.member` nodeAnnotations = Just Expr
                            | ("HsSpliceTy", "HsType") `Set.member` nodeAnnotations = Just HsType
                            | ("SpliceD", "HsDecl") `Set.member` nodeAnnotations = Just HsDecl
                            | otherwise = Nothing
                    forM_ spCxt $ \spliceContext -> forM_ expandStyles $ \(_style, title, cmdId) -> do
                        let range = realSrcSpanToRange nodeSpan
                            params = ExpandSpliceParams {uri = theUri, ..}
                        act <-
                            liftIO $
                                mkLspCommand plId cmdId title (Just [toJSON params])
                        tell $
                            DL.singleton $
                                CACodeAction $
                                    CodeAction title (Just CodeActionRefactorRewrite) Nothing Nothing (Just act)

isSpliceNode :: HieAST Type -> Bool
isSpliceNode Node {..} =
    not $
        Set.null $
            spliceAnns
                `Set.intersection` nodeAnnotations nodeInfo

spliceAnns :: Set (FastString, FastString)
spliceAnns =
    Set.fromList
        [ ("SplicePat", "Pat")
        , ("HsSpliceE", "HsExpr")
        , ("HsSpliceTy", "HsType")
        , ("SpliceD", "HsDecl")
        ]

expandStyles :: [(ExpandStyle, T.Text, CommandId)]
expandStyles =
    [ (Inplace, inplaceCmdName, expandInplaceId)
    , (Commented, commentedCmdName, expandCommentedId)
    ]
