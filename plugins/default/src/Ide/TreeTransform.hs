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
import           Data.Functor.Identity
import qualified Data.Text as T
import           Debug.Trace
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat  hiding (parseExpr)
import           Development.IDE.Types.Location
import           Generics.SYB
import           Ide.PluginUtils
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities (ClientCapabilities)
import           Outputable
import           Retrie.ExactPrint hiding (parseExpr)
import           Text.Regex.TDFA.Text()


------------------------------------------------------------------------------
-- | Get the latest version of the annotated parse source.
useAnnotatedSource
    :: String
    -> IdeState
    -> NormalizedFilePath
    -> IO (Maybe (Annotated ParsedSource))
useAnnotatedSource herald state nfp = do
  pm <- runAction herald state $ use GetParsedModule nfp
  pure $ fmap fixAnns pm


------------------------------------------------------------------------------
-- | A transformation for grafting source trees together. Use the semigroup
-- instance to combine 'Graft's, and run them via 'transform'.
newtype Graft a = Graft
  { runGraft :: DynFlags -> a -> Transform a
  }

instance Semigroup (Graft a) where
  Graft a <> Graft b = Graft $ \dflags -> a dflags >=> b dflags

instance Monoid (Graft a) where
  mempty = Graft $ const pure


------------------------------------------------------------------------------
-- | Convert a 'Graft' into a 'WorkspaceEdit'.
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


------------------------------------------------------------------------------
-- | Construct a 'Graft', replacing the node at the given 'SrcSpan' with the
-- given 'LHSExpr'. The node at that position must already be a 'LHsExpr', or
-- this is a no-op.
graft
    :: forall a
     . Data a
    => SrcSpan
    -> LHsExpr GhcPs
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


------------------------------------------------------------------------------
-- | Dark magic I stole from retrie. No idea what it does.
fixAnns :: ParsedModule -> Annotated ParsedSource
fixAnns ParsedModule {..} =
  let ranns = relativiseApiAnns pm_parsed_source pm_annotations
   in unsafeMkA pm_parsed_source ranns 0


------------------------------------------------------------------------------
-- | Given an 'LHSExpr', compute its exactprint annotations.
annotate :: DynFlags -> LHsExpr GhcPs -> Transform (Anns, LHsExpr GhcPs)
annotate dflags expr = do
  uniq <- show <$> uniqueSrcSpanT
  let rendered = traceId $ render dflags expr
      Right (anns, expr') = parseExpr dflags uniq rendered
      anns' = setPrecedingLines expr' 0 1 anns
  pure (anns', expr')


------------------------------------------------------------------------------
-- | Print out something 'Outputable'.
render :: Outputable a => DynFlags -> a -> String
render dflags = showSDoc dflags . ppr


------------------------------------------------------------------------------
-- | Put parentheses around an expression if required.
parenthesize :: LHsExpr GhcPs -> LHsExpr GhcPs
parenthesize = parenthesizeHsExpr appPrec

