{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

module Ide.Plugin.Class.Types where

import           Control.DeepSeq                  (rwhnf)
import           Control.Monad.Extra              (mapMaybeM, whenMaybe)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Maybe        (MaybeT (runMaybeT),
                                                   hoistMaybe)
import           Data.Aeson
import qualified Data.IntMap                      as IntMap
import           Data.Maybe                       (catMaybes, mapMaybe,
                                                   maybeToList)
import qualified Data.Text                        as T
import           Data.Unique                      (hashUnique, newUnique)
import           Development.IDE
import           Development.IDE.Core.PluginUtils (useMT)
import qualified Development.IDE.Core.Shake       as Shake
import           Development.IDE.GHC.Compat       hiding (newUnique, (<+>))
import           Development.IDE.GHC.Compat.Util  (bagToList)
import           Development.IDE.Graph.Classes
import           GHC.Generics
import           Ide.Plugin.Class.Utils
import           Ide.Types
import           Language.LSP.Protocol.Types      (TextEdit (TextEdit),
                                                   VersionedTextDocumentIdentifier)

typeLensCommandId :: CommandId
typeLensCommandId = "classplugin.typelens"

codeActionCommandId :: CommandId
codeActionCommandId = "classplugin.codeaction"

-- | Default indent size for inserting
defaultIndent :: Int
defaultIndent = 2

data AddMinimalMethodsParams = AddMinimalMethodsParams
    { verTxtDocId :: VersionedTextDocumentIdentifier
    , range       :: Range
    , methodGroup :: [(T.Text, T.Text)]
    -- ^ (name text, signature text)
    , withSig     :: Bool
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetInstanceBindTypeSigs = GetInstanceBindTypeSigs
    deriving (Generic, Show, Eq, Ord, Hashable, NFData)

data InstanceBindTypeSig = InstanceBindTypeSig
    { bindName     :: Name
    , bindRendered :: !T.Text
    , bindDefSpan  :: Maybe SrcSpan
    -- ^SrcSpan for the bind definition
    }

newtype InstanceBindTypeSigsResult =
    InstanceBindTypeSigsResult [InstanceBindTypeSig]

instance Show InstanceBindTypeSigsResult where
    show _ = "<InstanceBindTypeSigs>"

instance NFData InstanceBindTypeSigsResult where
    rnf = rwhnf

type instance RuleResult GetInstanceBindTypeSigs = InstanceBindTypeSigsResult

data InstanceBindLensCommand = InstanceBindLensCommand
    { commandUri :: Uri
    , commandUid :: Int}
    deriving (Generic, FromJSON, ToJSON)

data GetInstanceBindLens = GetInstanceBindLens
    deriving (Generic, Show, Eq, Ord, Hashable, NFData)

data InstanceBindLens = InstanceBindLens
    { lensRange             :: [(Range, Int)]
    , lensRendered          :: IntMap.IntMap TextEdit
    , lensEnabledExtensions :: [Extension]
    }

newtype InstanceBindLensResult =
    InstanceBindLensResult InstanceBindLens

instance Show InstanceBindLensResult where
    show _ = "<InstanceBindLens>"

instance NFData InstanceBindLensResult where
    rnf = rwhnf

type instance RuleResult GetInstanceBindLens = InstanceBindLensResult

data Log
  = LogImplementedMethods Class [T.Text]
  | LogShake Shake.Log

instance Pretty Log where
  pretty = \case
    LogImplementedMethods cls methods ->
      pretty ("Detected implemented methods for class" :: String)
        <+> pretty (show (getOccString cls) <> ":") -- 'show' is used here to add quotes around the class name
        <+> pretty methods
    LogShake log -> pretty log

data BindInfo = BindInfo
    { bindSpan     :: SrcSpan
      -- ^ SrcSpan of the whole binding
    , bindNameSpan :: SrcSpan
      -- ^ SrcSpan of the binding name
    }

getInstanceBindLensRule :: Recorder (WithPriority Log) -> Rules ()
getInstanceBindLensRule recorder = do
    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetInstanceBindLens nfp -> runMaybeT $ do
        tmr@(tmrRenamed ->  (hs_tyclds -> tycls, _, _, _)) <- useMT TypeCheck nfp
        (InstanceBindTypeSigsResult allBinds) <- useMT GetInstanceBindTypeSigs nfp

        let -- declared instance methods without signatures
            bindInfos = [ bind
                        | instds <- map group_instds tycls -- class instance decls
                        , instd <- instds
                        , inst <- maybeToList $ getClsInstD (unLoc instd)
                        , bind <- getBindSpanWithoutSig inst
                        ]
            targetSigs = matchBind bindInfos allBinds
        rangeIntText <- liftIO $ mapMaybeM getRangeWithSig targetSigs
        let lensRange = (\(range, int, _) -> (range, int)) <$> rangeIntText
            lensRendered = IntMap.fromList $ (\(range, int, text) -> (int, TextEdit range text)) <$> rangeIntText
            lensEnabledExtensions = getExtensions $ tmrParsed tmr
        pure $ InstanceBindLensResult $ InstanceBindLens{..}
    where
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
                        -- here we filter out nonsense generated bindings
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
        getRangeWithSig :: InstanceBindTypeSig -> IO (Maybe (Range, Int, T.Text))
        getRangeWithSig bind = runMaybeT $ do
            span <- hoistMaybe $ bindDefSpan bind
            range <- hoistMaybe $ srcSpanToRange span
            uniqueID <- liftIO $ hashUnique <$> newUnique
            pure (range, uniqueID, bindRendered bind)



getInstanceBindTypeSigsRule :: Recorder (WithPriority Log) -> Rules ()
getInstanceBindTypeSigsRule recorder = do
    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetInstanceBindTypeSigs nfp -> runMaybeT $ do
        (tmrTypechecked -> gblEnv ) <- useMT TypeCheck nfp
        (hscEnv -> hsc) <- useMT GhcSession nfp
        let binds = collectHsBindsBinders $ tcg_binds gblEnv
        (_, maybe [] catMaybes -> instanceBinds) <- liftIO $
            initTcWithGbl hsc gblEnv ghostSpan $ traverse (bindToSig hsc gblEnv) binds
        pure $ InstanceBindTypeSigsResult instanceBinds
    where
        rdrEnv gblEnv= tcg_rdr_env gblEnv
        showDoc hsc gblEnv ty = showSDocForUser' hsc (mkPrintUnqualifiedDefault hsc (rdrEnv gblEnv)) (pprSigmaType ty)
        bindToSig hsc gblEnv id = do
            let name = idName id
            whenMaybe (isBindingName name) $ do
                env <- tcInitTidyEnv
                let (_, ty) = tidyOpenType env (idType id)
                pure $ InstanceBindTypeSig name
                        (prettyBindingNameString (printOutputable name) <> " :: " <> T.pack (showDoc hsc gblEnv ty))
                        Nothing
