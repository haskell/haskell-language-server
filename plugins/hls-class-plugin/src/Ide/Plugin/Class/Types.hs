{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Class.Types where

import           Control.DeepSeq                  (rwhnf)
import           Control.Monad.Extra              (mapMaybeM)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Data.Aeson
import qualified Data.IntMap                      as IntMap
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe, maybeToList)
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
import qualified Ide.Plugin.RangeMap              as RangeMap
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

-- | Indexes the instances declared in a module by their source span, giving
-- each instance's class and the full instantiated type of every class method.
-- Both the placeholder code action (to enumerate missing methods) and the
-- code lens (to display inferred signatures) consume this rule.
data GetClassInstances = GetClassInstances
    deriving (Generic, Show, Eq, Ord, Hashable, NFData)

data InstanceInfo = InstanceInfo
    { instSpan    :: SrcSpan
      -- ^ Source span of the instance declaration.
    , instClass   :: Class
    , instMethods :: [(Name, Type)]
      -- ^ Each class method paired with its type instantiated for this
      -- instance, including any instance context (e.g. @Eq a@ for
      -- @instance Eq a => C [a]@).
    }

newtype ClassInstancesResult =
    ClassInstancesResult (RangeMap.RangeMap InstanceInfo)

instance Show ClassInstancesResult where
    show _ = "<ClassInstances>"

instance NFData ClassInstancesResult where
    rnf = rwhnf

type instance RuleResult GetClassInstances = ClassInstancesResult

type instance RuleResult GetInstanceBindTypeSigs = InstanceBindTypeSigsResult
type instance RuleInput GetInstanceBindTypeSigs = ProjectHaskellFiles

-- |The necessary data to execute our code lens
data InstanceBindLensCommand = InstanceBindLensCommand
    { -- |The URI needed to run actions in the command
      commandUri  :: Uri
      -- |The specific TextEdit we want to apply. This does not include the
      -- pragma edit which is computed in the command
    , commandEdit :: TextEdit }
    deriving (Generic, FromJSON, ToJSON)

-- | The InstanceBindLens rule is specifically for code lenses. It correlates
-- user-written instance method bindings (those without an explicit signature)
-- to the instance's 'InstanceInfo', and emits range/name/type triples with
-- unique IDs for resolve.
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
type instance RuleInput GetInstanceBindLens = ProjectHaskellFiles

data Log
  = LogImplementedMethods DynFlags Class ClassMinimalDef
  | LogShake Shake.Log

instance Pretty Log where
  pretty = \case
    LogImplementedMethods dflags cls methods ->
      pretty ("The following methods are missing" :: String)
        <+> pretty (show (getOccString cls) <> ":") -- 'show' is used here to add quotes around the class name
        <+> pretty (showSDoc dflags $ ppr methods)
    LogShake log -> pretty log

-- | A user-written instance binding without an explicit signature. The 'Name'
-- is the renamer-level method name (e.g. @(==)@), used to look up the
-- instance-level type in 'instMethods'.
data BindInfo = BindInfo
    { bindSpan    :: SrcSpan
      -- ^ SrcSpan of the whole binding
    , bindFunName :: Name
      -- ^ The renamed method name of the binding.
    }

getInstanceBindLensRule :: Recorder (WithPriority Log) -> Rules ()
getInstanceBindLensRule recorder = do
    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetInstanceBindLens nfp -> runMaybeT $ do
#if MIN_VERSION_ghc(9,9,0)
        tmr@(tmrRenamed ->  (hs_tyclds -> tycls, _, _, _, _)) <- useMT TypeCheck nfp
#else
        tmr@(tmrRenamed ->  (hs_tyclds -> tycls, _, _, _)) <- useMT TypeCheck nfp
#endif
        ClassInstancesResult instMap <- useMT GetClassInstances nfp

        let -- Correlate renamed ClsInstDecls with their InstanceInfo by source
            -- span (the only link between the renamed tree and tcg_insts), then
            -- collect user-written bindings that lack an explicit signature.
            entries =
                [ (bind, ty)
                | instds <- map group_instds tycls
                , instd <- instds
                , inst <- maybeToList $ getClsInstD (unLoc instd)
                , info <- maybeToList $ do
                    instdRange <- srcSpanToRange (getLocA instd)
                    listToMaybe (RangeMap.elementsInRange instdRange instMap)
                , bind <- getBindSpanWithoutSig inst
                , ty <- maybeToList (lookup (bindFunName bind) (instMethods info))
                ]
        rangeIntNameType <- liftIO $ mapMaybeM tagEntry entries
        let lensRange = (\(range, int, _, _) -> (range, int)) <$> rangeIntNameType
            lensDetails = IntMap.fromList $
                (\(range, int, name, typ) -> (int, (range, name, typ))) <$> rangeIntNameType
            lensEnabledExtensions = getExtensions $ tmrParsed tmr
        pure $ InstanceBindLensResult $ InstanceBindLens{..}
    where
        tagEntry (bind, ty) = case srcSpanToRange (bindSpan bind) of
            Nothing -> pure Nothing
            Just r  -> do
                uniqueID <- hashUnique <$> newUnique
                pure $ Just (r, uniqueID, bindFunName bind, ty)

        getClsInstD (ClsInstD _ d) = Just d
        getClsInstD _              = Nothing

        getSigName (ClassOpSig _ _ sigNames _) = Just $ map unLoc sigNames
        getSigName _                           = Nothing

        getBindSpanWithoutSig :: ClsInstDecl GhcRn -> [BindInfo]
        getBindSpanWithoutSig ClsInstDecl{..} =
            let bindNames = mapMaybe go $
#if !MIN_VERSION_ghc(9,11,0)
                              bagToList
#endif
                                cid_binds
                go (L l bind) = case bind of
                    FunBind{..}
                      -- `Generated` tagged for Template Haskell,
                      -- here we filter out nonsense generated bindings
                      -- that are nonsense for displaying code lenses.
                      --
                      -- See https://github.com/haskell/haskell-language-server/issues/3319
                      | not $ isGenerated (groupOrigin fun_matches)
                      -> Just $ L l fun_id
                    _ -> Nothing
                -- Existed signatures' name
                existingSigNames = concat $ mapMaybe (\(L _ r) -> getSigName r) cid_sigs
                toBindInfo (L l (L _ n)) = BindInfo (locA l) n
            in toBindInfo <$> filter (\(L _ name) -> unLoc name `notElem` existingSigNames) bindNames

getClassInstancesRule :: Recorder (WithPriority Log) -> Rules ()
getClassInstancesRule recorder = do
    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetClassInstances nfp -> runMaybeT $ do
        (tmrTypechecked -> gblEnv) <- useMT TypeCheck nfp
        (hscEnv -> hsc) <- useMT GhcSession nfp
        (_, mInfos) <- liftIO $
            initTcWithGbl hsc gblEnv ghostSpan
#if MIN_VERSION_ghc(9,7,0)
              $ liftZonkM
#endif
              $ mkInfos gblEnv
        pure $ ClassInstancesResult $ RangeMap.fromList'
            $ mapMaybe (\i -> (,i) <$> srcSpanToRange (instSpan i)) (fromMaybe [] mInfos)
    where
        mkInfos gblEnv = do
            env <- tcInitTidyEnv
            pure $ map (mkInfo env) (tcg_insts gblEnv)

        mkInfo env inst = do
            -- forall tvs. theta => cls tys
            let (_tvs, theta, cls, tys) = instanceSig inst
                -- Canonicalise internal GHC type-variable names (e.g. a_1 -> a).
                tidy ty =
#if MIN_VERSION_ghc(9,11,0)
                    tidyOpenType env ty
#else
                    snd (tidyOpenType env ty)
#endif
                -- `instantiateMethod` substitutes the instance head types into
                -- the method's type and drops the leading class predicate, but
                -- not the instance's own constraints (`theta`). Re-prepend them
                -- so we get every method's full instantiated type.
                mkMeth m = (idName m, tidy (mkInvisFunTys theta (instantiateMethod cls m tys)))
            InstanceInfo
              { instSpan    = getSrcSpan (is_dfun inst)
              , instClass   = cls
              , instMethods = map mkMeth (classMethods cls)
              }
