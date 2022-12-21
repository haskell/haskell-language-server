{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE BangPatterns     #-}

module Ide.Plugin.Class.Types where

import           Control.DeepSeq               (rwhnf)
import           Control.Monad.Extra           (whenMaybe)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson
import           Data.Maybe                    (catMaybes)
import qualified Data.Text                     as T
import           Development.IDE
import qualified Development.IDE.Core.Shake    as Shake
import           Development.IDE.GHC.Compat    hiding ((<+>))
import           Development.IDE.Graph.Classes
import           GHC.Generics
import           Ide.Plugin.Class.Utils
import           Ide.Types

typeLensCommandId :: CommandId
typeLensCommandId = "classplugin.typelens"

codeActionCommandId :: CommandId
codeActionCommandId = "classplugin.codeaction"

-- | Default indent size for inserting
defaultIndent :: Int
defaultIndent = 2

data AddMinimalMethodsParams = AddMinimalMethodsParams
    { uri         :: Uri
    , range       :: Range
    , methodGroup :: List (T.Text, T.Text)
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

rules :: Recorder (WithPriority Log) -> Rules ()
rules recorder = do
    define (cmapWithPrio LogShake recorder) $ \GetInstanceBindTypeSigs nfp -> do
        tmr <- use TypeCheck nfp
        hsc <- use GhcSession nfp
        result <- liftIO $ instanceBindType (hscEnv <$> hsc) (tmrTypechecked <$> tmr)
        pure ([], result)
    where
        instanceBindType :: Maybe HscEnv -> Maybe TcGblEnv -> IO (Maybe InstanceBindTypeSigsResult)
        instanceBindType (Just hsc) (Just gblEnv) = do
            let binds = collectHsBindsBinders $ tcg_binds gblEnv
            (_, maybe [] catMaybes -> instanceBinds) <-
                initTcWithGbl hsc gblEnv ghostSpan $ traverse bindToSig binds
            pure $ Just $ InstanceBindTypeSigsResult instanceBinds
            where
                rdrEnv = tcg_rdr_env gblEnv
                showDoc ty = showSDocForUser' hsc (mkPrintUnqualifiedDefault hsc rdrEnv) (pprSigmaType ty)

                bindToSig id = do
                    let name = idName id
                    whenMaybe (isBindingName name) $ do
                        env <- tcInitTidyEnv
                        let (_, ty) = tidyOpenType env (idType id)
                        pure $ InstanceBindTypeSig name
                                (prettyBindingNameString (printOutputable name) <> " :: " <> T.pack (showDoc ty))
                                Nothing
        instanceBindType _ _ = pure Nothing
