{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.TreeTransform
  ( Graft, graft, transform, useAnnotatedSource
  ) where

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
  let inside_app =
        everything (||)
          (mkQ False $ \case
            (L _ (HsApp _ (L src _) _) :: LHsExpr GhcPs)
              | src == dst -> True
            (L _ (HsApp _ _ (L src _)) :: LHsExpr GhcPs)
              | src == dst -> True
            _ -> False
          ) a
  (anns, val') <- annotate dflags $ bool val (parenthesize val) inside_app
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
parenthesize a@(L _ HsVar{})        = a
parenthesize a@(L _ HsUnboundVar{}) = a
parenthesize a@(L _ HsOverLabel{})  = a
parenthesize a@(L _ HsOverLit{})    = a
parenthesize a@(L _ HsIPVar{})      = a
parenthesize a@(L _ HsLit{})        = a
parenthesize a = noLoc $ HsPar NoExt a

