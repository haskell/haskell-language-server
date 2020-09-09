{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.TreeTransform
  ( Graft, graft, transform, useAnnotatedSource
  ) where

import           BasicTypes (appPrec)
import           Control.Monad
import           Data.Bool
import           Data.Functor.Identity
import qualified Data.Text as T
import           Debug.Trace
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           GHC hiding (parseExpr)
import           Generics.SYB
import           Ide.PluginUtils
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities (ClientCapabilities)
import           Outputable
import           Retrie.ExactPrint hiding (parseExpr)
import           Text.Regex.TDFA.Text()


useAnnotatedSource :: String -> IdeState -> NormalizedFilePath -> IO (Annotated ParsedSource)
useAnnotatedSource herald state nfp = do
  Just pm <- runAction herald state $ use GetParsedModule nfp
  pure $ fixAnns pm


newtype Graft a = Graft
  { runGraft :: DynFlags -> a -> Transform a
  }

instance Semigroup (Graft a) where
  Graft a <> Graft b = Graft $ \dflags -> a dflags >=> b dflags

instance Monoid (Graft a) where
  mempty = Graft $ const pure


transform
    :: DynFlags
    -> ClientCapabilities
    -> Uri
    -> Graft ParsedSource
    -> Annotated ParsedSource
    -> WorkspaceEdit
transform dflags ccs uri f a =
  let src = printA a
      a' = runIdentity $ transformA a $ runGraft f dflags
      res = printA a'
   in diffText ccs (uri, T.pack src) (T.pack res) IncludeDeletions


graft
    :: forall a
     . (Data a)
    => SrcSpan
    -> Located (HsExpr GhcPs)
    -> Graft a
graft dst val = Graft $ \dflags a -> do
  (anns, val') <- annotate dflags $ parenthesize val
  modifyAnnsT $ mappend anns
  pure $ everywhere'
    ( mkT $
        \case
          (L src _ :: LHsExpr GhcPs) | src == dst -> val'
          l -> l
    ) a


fixAnns :: ParsedModule -> Annotated ParsedSource
fixAnns ParsedModule {..} =
  let ranns = relativiseApiAnns pm_parsed_source pm_annotations
   in unsafeMkA pm_parsed_source ranns 0


annotate :: DynFlags -> LHsExpr GhcPs -> Transform (Anns, LHsExpr GhcPs)
annotate dflags expr = do
  uniq <- show <$> uniqueSrcSpanT
  let rendered = traceId $ render dflags expr
      Right (anns, expr') = parseExpr dflags uniq rendered
      anns' = setPrecedingLines expr' 0 1 anns
  pure (anns', expr')

render :: Outputable a => DynFlags -> a -> String
render dflags = showSDoc dflags . ppr

------------------------------------------------------------------------------
-- | Put parentheses around an expression if required.
parenthesize :: LHsExpr GhcPs -> LHsExpr GhcPs
parenthesize = parenthesizeHsExpr appPrec

