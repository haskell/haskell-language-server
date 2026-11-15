{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Retrie.Utils
  ( findBinder
  , definedAt
  , definedAs
  , qualify
  , getLocationNfp
  , getLocationSpan
  , getLocationRange
  ) where

import           Control.Lens               ((^.))
import           Data.Data                  (Data, Typeable)
import           Data.Generics              (everything, mkQ)
import           Data.Monoid                (First (First), getFirst)
import qualified Data.Text                  as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           GHC.Parser.Annotation      (LocatedN)
import qualified Language.LSP.Protocol.Lens as L


-- | Find a matching binder anywhere in a parsed or renamed AST.
findBinder
  :: forall p a. (Data a, Typeable p)
  => (LIdP (GhcPass p) -> Bool)
  -> a
  -> Maybe ( LIdP (GhcPass p), MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) )
findBinder predicate =
    getFirst <$> everything (<>) (First Nothing `mkQ` matcher)
  where
    matcher
      :: HsBindLR (GhcPass p) (GhcPass p)
      -> First ( LIdP (GhcPass p), MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) )
    matcher FunBind{fun_id, fun_matches}
      | predicate fun_id = First $ Just (fun_id, fun_matches)
    matcher _ = First Nothing

-- | Predicate that matches a binder at a specific span.
definedAt :: RealSrcSpan -> LocatedN a -> Bool
definedAt sp fun_id
  | RealSrcSpan sp' _ <- getLocA fun_id = sp' == sp
  | otherwise = False

-- | Predicate that matches a top-level binder with the given name.
definedAs :: String -> LIdP GhcRn -> Bool
definedAs qualified (L _ name) = case nameModule_maybe name of
  Just m  -> qualify m (T.unpack $ printOutputable name) == qualified
  Nothing -> False

-- | Print the name qualified.
qualify :: Outputable mod => mod -> String -> String
qualify ms_mod x = T.unpack (printOutputable ms_mod) <> "." <> x

getLocationRange :: Location -> Range
getLocationRange location = location ^. L.range

getLocationNfp :: Location -> NormalizedFilePath
getLocationNfp loc = fromUri $ toNormalizedUri $ loc ^. L.uri

getLocationSpan :: Location -> RealSrcSpan
getLocationSpan loc = rangeToRealSrcSpan nfp $ getLocationRange loc
  where
    nfp = getLocationNfp loc
