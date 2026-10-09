{-# LANGUAGE RecordWildCards #-}
module Plugin (plugin) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable          (for_)
import Data.List              (foldl')
import Data.List.NonEmpty     (NonEmpty (..))
import Data.Traversable       (for)

import qualified Data.Generics as SYB

import qualified GHC.Plugins  as GHC
import           GHC

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.parsedResultAction = \_cliOptions -> pluginImpl
    }

pluginImpl :: GHC.ModSummary -> GHC.ParsedResult -> GHC.Hsc GHC.ParsedResult
pluginImpl _modSummary pm = do
    let m = GHC.parsedResultModule pm
    hpm_module' <- transform (GHC.hpm_module m)
    let module' = m { GHC.hpm_module = hpm_module' }
    return pm { GHC.parsedResultModule = module' }

transform
    :: GHC.Located (HsModule GhcPs)
    -> GHC.Hsc (GHC.Located (HsModule GhcPs))
transform = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcPs -> GHC.Hsc (LHsExpr GhcPs)
    transform' expr@(L srcSpan (HsVar _ lvar)) =
        if GHC.occNameString (GHC.occName $ GHC.unLoc lvar) == "pluginConstant"
            then return (nlHsIntLit 0x42)
            else return expr
    transform' expr =
        return expr
