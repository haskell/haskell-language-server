{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shake rules used in this plugin.
module Ide.Plugin.Retrie.Rules
  ( GetFixedParsedModule(..)
  , getFixedParsedModuleRule
  , fixityEnvFor
  , fixAnns
  ) where

import           Control.DeepSeq
import           Data.Hashable
import qualified Data.Map                   as Map
import           Development.IDE
import           Development.IDE.GHC.Compat
import           GHC.Generics               (Generic)

import           Retrie
import           Retrie.ExactPrint
import           Retrie.Fixity

import           Ide.Plugin.Retrie.Log


data GetFixedParsedModule = GetFixedParsedModule
  deriving (Eq, Show, Generic)
instance Hashable GetFixedParsedModule
instance NFData GetFixedParsedModule

type instance RuleResult GetFixedParsedModule = AnnotatedModule

instance Show (Annotated ast) where show _ = "<Annotated>"
instance NFData (Annotated ast) where rnf = rwhnf

-- | Rule that reassociates the parsed module with the fixities from
-- typechecking.
getFixedParsedModuleRule :: Recorder (WithPriority Log) -> Rules ()
getFixedParsedModuleRule recorder =
  define (cmapWithPrio LogShake recorder) $ \ GetFixedParsedModule nfp -> do
    mpm <- use GetParsedModuleWithComments nfp
    mfm <- use GetFixity nfp
    case (mpm, mfm) of
      (Just pm, Just fm) -> do
        m <- transformA (fixAnns pm) (fix $ fixityEnvFor fm)
        pure ([], Just m)
      _ -> pure ([], Nothing)

fixityEnvFor :: FixityMap -> FixityEnv
fixityEnvFor (FixityMap fixities) =
  -- retrie keys fixities by the operator's unqualified name
  mkFixityEnv
    [ (fs, (fs, fixity))
    | (n, fixity) <- Map.toList fixities
    , let fs = occNameFS (nameOccName n)
    ]

fixAnns :: ParsedModule -> Annotated ParsedSource
fixAnns ParsedModule{ pm_parsed_source } =
  unsafeMkA (makeDeltaAst pm_parsed_source) 0
