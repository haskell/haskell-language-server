{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Class.Types where

import           Control.DeepSeq                  (rwhnf)
import           Control.Monad.Extra              (mapMaybeM, whenMaybe)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import           Data.Aeson
import qualified Data.IntMap                      as IntMap
import           Data.List.Extra                  (firstJust)
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
import           Language.LSP.Protocol.Types      (TextEdit,
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

-- |The InstanceBindTypeSigs Rule collects the instance bindings type
-- signatures (both name and type). It is used by both the code actions and the
-- code lenses
data GetInstanceBindTypeSigs = GetInstanceBindTypeSigs
    deriving (Generic, Show, Eq, Ord, Hashable, NFData)

data InstanceBindTypeSig = InstanceBindTypeSig
    { bindName :: Name
    , bindType :: Type
    }

newtype InstanceBindTypeSigsResult =
    InstanceBindTypeSigsResult [InstanceBindTypeSig]

instance Show InstanceBindTypeSigsResult where
    show _ = "<InstanceBindTypeSigs>"

instance NFData InstanceBindTypeSigsResult where
    rnf = rwhnf

type instance RuleResult GetInstanceBindTypeSigs = InstanceBindTypeSigsResult

-- |The necessary data to execute our code lens
data InstanceBindLensCommand = InstanceBindLensCommand
    { -- |The URI needed to run actions in the command
      commandUri  :: Uri
      -- |The specific TextEdit we want to apply. This does not include the
      -- pragma edit which is computed in the command
    , commandEdit :: TextEdit }
    deriving (Generic, FromJSON, ToJSON)

-- | The InstanceBindLens rule is specifically for code lenses. It  relies on
-- the InstanceBindTypeSigs rule, filters out irrelevant matches and signatures
-- that can't be matched to a source span. It provides all the signatures linked
-- to a unique ID to aid in resolving. It also provides a list of enabled
-- extensions.
data GetInstanceBindLens = GetInstanceBindLens
    deriving (Generic, Show, Eq, Ord, Hashable, NFData)

data InstanceBindLens = InstanceBindLens
    { -- |What we need to provide the code lens. The range linked with
      -- a unique ID that will allow us to resolve the rest of the data later
      lensRange             :: [(Range, Int)]
      -- |Provides the necessary data to allow us to display the
      -- title of the lens and compute a TextEdit for it.
    , lensDetails           :: IntMap.IntMap (Range, Name, Type)
    -- |Provides currently enabled extensions, allowing us to conditionally
    -- insert needed extensions.
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
#if MIN_VERSION_ghc(9,9,0)
        tmr@(tmrRenamed ->  (hs_tyclds -> tycls, _, _, _, _)) <- useMT TypeCheck nfp
#else
        tmr@(tmrRenamed ->  (hs_tyclds -> tycls, _, _, _)) <- useMT TypeCheck nfp
#endif
        (InstanceBindTypeSigsResult allBinds) <- useMT GetInstanceBindTypeSigs nfp

        let -- declared instance methods without signatures
            bindInfos = [ bind
                        | instds <- map group_instds tycls -- class instance decls
                        , instd <- instds
                        , inst <- maybeToList $ getClsInstD (unLoc instd)
                        , bind <- getBindSpanWithoutSig inst
                        ]
            targetSigs = matchBind bindInfos allBinds
        rangeIntNameType <- liftIO $ mapMaybeM getRangeWithSig targetSigs
        let lensRange = (\(range, int, _, _) -> (range, int)) <$> rangeIntNameType
            lensDetails = IntMap.fromList $ (\(range, int, name, typ) -> (int, (range, name, typ))) <$> rangeIntNameType
            lensEnabledExtensions = getExtensions $ tmrParsed tmr
        pure $ InstanceBindLensResult $ InstanceBindLens{..}
    where
        -- Match Binds with their signatures
        -- We try to give every `InstanceBindTypeSig` a `SrcSpan`,
        -- hence we can display signatures for `InstanceBindTypeSig` with span later.
        matchBind :: [BindInfo] -> [InstanceBindTypeSig] -> [Maybe (InstanceBindTypeSig, SrcSpan)]
        matchBind existedBinds allBindWithSigs =
            [firstJust (go bindSig) existedBinds | bindSig <- allBindWithSigs]
            where
                go :: InstanceBindTypeSig -> BindInfo -> Maybe (InstanceBindTypeSig,  SrcSpan)
                go bindSig bind = do
                    range <- (srcSpanToRange . bindNameSpan) bind
                    if inRange range (getSrcSpan $ bindName bindSig)
                    then Just (bindSig, bindSpan bind)
                    else Nothing

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

        -- Get bind definition range with its rendered signature text
        getRangeWithSig :: Maybe (InstanceBindTypeSig, SrcSpan) -> IO (Maybe (Range, Int, Name, Type))
        getRangeWithSig (Just (bind, span)) = runMaybeT $ do
            range <- MaybeT . pure $ srcSpanToRange span
            uniqueID <- liftIO $ hashUnique <$> newUnique
            pure (range, uniqueID, bindName bind, bindType bind)
        getRangeWithSig Nothing = pure Nothing


getInstanceBindTypeSigsRule :: Recorder (WithPriority Log) -> Rules ()
getInstanceBindTypeSigsRule recorder = do
    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetInstanceBindTypeSigs nfp -> runMaybeT $ do
        (tmrTypechecked -> gblEnv ) <- useMT TypeCheck nfp
        (hscEnv -> hsc) <- useMT GhcSession nfp
        let binds = collectHsBindsBinders $ tcg_binds gblEnv
        (_, maybe [] catMaybes -> instanceBinds) <- liftIO $
            initTcWithGbl hsc gblEnv ghostSpan
#if MIN_VERSION_ghc(9,7,0)
              $ liftZonkM
#endif
              $ traverse bindToSig binds
        pure $ InstanceBindTypeSigsResult instanceBinds
    where
        bindToSig id = do
            let name = idName id
            whenMaybe (isBindingName name) $ do
                env <- tcInitTidyEnv
                let (_, ty) = tidyOpenType env (idType id)
                pure $ InstanceBindTypeSig name ty
