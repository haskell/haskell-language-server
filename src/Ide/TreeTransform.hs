{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.TreeTransform
  ( Graft, graft, transform, useAnnotatedSource
  ) where

import Control.Monad
import Data.Functor.Identity
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import Development.IDE.Types.Location
import GHC
import Generics.SYB
import Ide.PluginUtils
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities (ClientCapabilities)
import Retrie.ExactPrint
import Text.Regex.TDFA.Text()
import qualified Data.Text as T


useAnnotatedSource :: String -> IdeState -> NormalizedFilePath -> IO (Annotated ParsedSource)
useAnnotatedSource herald state nfp = do
  Just pm <- runAction herald state $ use GetParsedModule nfp
  pure $ fixAnns pm


newtype Graft a = Graft
  { runGraft :: a -> Transform a
  }

instance Semigroup (Graft a) where
  Graft a <> Graft b = Graft $ a >=> b

instance Monoid (Graft a) where
  mempty = Graft pure


transform
    :: ClientCapabilities
    -> Uri
    -> Graft ParsedSource
    -> Annotated ParsedSource
    -> WorkspaceEdit
transform ccs uri f a =
  let src = printA a
      a' = runIdentity $ transformA a $ runGraft f
      res = printA a'
   in diffText ccs (uri, T.pack src) (T.pack res) IncludeDeletions


graft
    :: forall a b
     . (Data a, Annotate b)
    => SrcSpan
    -> Located b
    -> Graft a
graft dst (L _ val) = Graft $ \a -> do
  span <- uniqueSrcSpanT
  let val' = L span val
  modifyAnnsT $ addAnnotationsForPretty [] val'
  pure $ everywhere ( mkT $ \case
    L src (_ :: b) | src == dst -> val'
    l -> l) a


fixAnns :: ParsedModule -> Annotated ParsedSource
fixAnns ParsedModule {..} =
  let ranns = relativiseApiAnns pm_parsed_source pm_annotations
   in unsafeMkA pm_parsed_source ranns 0



