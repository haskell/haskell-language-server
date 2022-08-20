{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use nubOrdOn" #-}

module Ide.Plugin.ExplicitFixity(descriptor) where

import           Control.DeepSeq
import           Control.Monad                        (forM)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Coerce                          (coerce)
import           Data.Either.Extra
import           Data.Hashable
import           Data.List.Extra                      (nubOn)
import qualified Data.Map                             as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers,
                                                       pluginRules)
import           Development.IDE.Core.PositionMapping (idDelta)
import           Development.IDE.Core.Shake           (addPersistentRule)
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util      (FastString)
import qualified Development.IDE.GHC.Compat.Util      as Util
import           Development.IDE.LSP.Notifications    (ghcideNotificationsPluginPriority)
import           GHC.Generics                         (Generic)
import           Ide.PluginUtils                      (getNormalizedFilePath,
                                                       handleMaybeM,
                                                       pluginResponse)
import           Ide.Types                            hiding (pluginId)
import           Language.LSP.Types

pluginId :: PluginId
pluginId = "explicitFixity"

descriptor :: Recorder (WithPriority Log) -> PluginDescriptor IdeState
descriptor recorder = (defaultPluginDescriptor pluginId)
    { pluginRules = fixityRule recorder
    , pluginHandlers = mkPluginHandler STextDocumentHover hover
    -- Make this plugin has a lower priority than ghcide's plugin to ensure
    -- type info display first.
    , pluginPriority = ghcideNotificationsPluginPriority - 1
    }

hover :: PluginMethodHandler IdeState TextDocumentHover
hover state _ (HoverParams (TextDocumentIdentifier uri) pos _) = pluginResponse $ do
    nfp <- getNormalizedFilePath uri
    fixityTrees <- handleMaybeM "ExplicitFixity: Unable to get fixity"
        $ liftIO
        $ runAction "ExplicitFixity.GetFixity" state
        $ use GetFixity nfp
    -- We don't have much fixities on one position, so `nubOn` is acceptable.
    pure $ toHover $ nubOn snd $ findInTree fixityTrees pos fNodeFixty
    where
        toHover :: [(T.Text, Fixity)] -> Maybe Hover
        toHover [] = Nothing
        toHover fixities =
            let -- Splicing fixity info
                contents = T.intercalate "\n\n" $ fixityText <$> fixities
                -- Append to the previous hover content
                contents' = "\n" <> sectionSeparator <> contents
            in  Just $ Hover (HoverContents $ unmarkedUpContent contents') Nothing

        fixityText :: (T.Text, Fixity) -> T.Text
        fixityText (name, Fixity _ precedence direction) =
            printOutputable direction <> " " <> printOutputable precedence <> " `" <> name <> "`"

-- | Transferred from ghc `selectSmallestContaining`
selectSmallestContainingForFixityTree :: Span -> FixityTree -> Maybe FixityTree
selectSmallestContainingForFixityTree sp node
    | sp `containsSpan` fNodeSpan node = Just node
    | fNodeSpan node `containsSpan` sp = getFirst $ mconcat
        [ foldMap (First . selectSmallestContainingForFixityTree sp) $ fNodeChildren node
        , First (Just node)
        ]
    | otherwise = Nothing

-- | Transferred from ghcide `pointCommand`
findInTree :: FixityTrees -> Position -> (FixityTree -> [a]) -> [a]
findInTree tree pos k =
    concat $ M.elems $ flip M.mapWithKey tree $ \fs ast ->
        maybe [] k (selectSmallestContainingForFixityTree (sp fs) ast)
    where
        sloc fs = mkRealSrcLoc fs (fromIntegral $ line+1) (fromIntegral $ cha+1)
        sp fs = mkRealSrcSpan (sloc fs) (sloc fs)
        line = _line pos
        cha = _character pos

data FixityTree = FNode
    { fNodeSpan     :: Span
    , fNodeChildren :: [FixityTree]
    , fNodeFixty    :: [(T.Text, Fixity)]
    } deriving (Generic)

instance NFData FixityTree where
    rnf = rwhnf

instance Show FixityTree where
    show _ = "<FixityTree>"

type FixityTrees = M.Map FastString FixityTree

newtype Log = LogShake Shake.Log

instance Pretty Log where
    pretty = \case
        LogShake log -> pretty log

data GetFixity = GetFixity deriving (Show, Eq, Generic)

instance Hashable GetFixity
instance NFData GetFixity

type instance RuleResult GetFixity = FixityTrees

fakeFixityTrees :: FixityTrees
fakeFixityTrees = M.empty

-- | Convert a HieASTs to FixityTrees with fixity info gathered
hieAstsToFixitTrees :: MonadIO m => HscEnv -> TcGblEnv -> HieASTs a -> m FixityTrees
hieAstsToFixitTrees hscEnv tcGblEnv ast =
    -- coerce to avoid compatibility issues.
    M.mapKeysWith const coerce <$>
        sequence (M.map (hieAstToFixtyTree hscEnv tcGblEnv) (getAsts ast))

-- | Convert a HieAST to FixityTree with fixity info gathered
hieAstToFixtyTree :: MonadIO m => HscEnv -> TcGblEnv -> HieAST a -> m FixityTree
hieAstToFixtyTree hscEnv tcGblEnv ast = case ast of
    (Node _ span []) -> FNode span [] <$> getFixities
    (Node _ span children) -> do
        fixities <- getFixities
        childrenFixities <- mapM (hieAstToFixtyTree hscEnv tcGblEnv) children
        pure $ FNode span childrenFixities fixities
    where
        -- Names at the current ast node
        names :: [Name]
        names = mapMaybe eitherToMaybe $ M.keys $ getNodeIds ast

        getFixities :: MonadIO m => m [(T.Text, Fixity)]
        getFixities = liftIO
            $ fmap (filter ((/= defaultFixity) . snd) . mapMaybe pickFixity)
            $ forM names $ \name ->
                (,) (printOutputable name)
                . snd
                <$> Util.handleGhcException
                    (const $ pure (emptyMessages, Nothing))
                    (initTcWithGbl hscEnv tcGblEnv (realSrcLocSpan $ mkRealSrcLoc "<dummy>" 1 1) (lookupFixityRn name))

        pickFixity :: (T.Text, Maybe Fixity) -> Maybe (T.Text, Fixity)
        pickFixity (_, Nothing)   = Nothing
        pickFixity (name, Just f) = Just (name, f)

fixityRule :: Recorder (WithPriority Log) -> Rules ()
fixityRule recorder = do
    define (cmapWithPrio LogShake recorder) $ \GetFixity nfp -> do
        HAR{hieAst} <- use_ GetHieAst nfp
        env <- hscEnv <$> use_ GhcSession nfp
        tcGblEnv <- tmrTypechecked <$> use_ TypeCheck nfp
        trees <- hieAstsToFixitTrees env tcGblEnv hieAst
        pure ([], Just trees)

    -- Ensure that this plugin doesn't block on startup
    addPersistentRule GetFixity $ \_ -> pure $ Just (fakeFixityTrees, idDelta, Nothing)
