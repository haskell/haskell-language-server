module Wingman.StaticPlugin
  ( staticPlugin
  , pattern WingmanMetaprogram
  ) where

import           Data.Data
import           Development.IDE.GHC.Compat
import           GHC.LanguageExtensions.Type (Extension(EmptyCase, QuasiQuotes))
import           Generics.SYB
import           GhcPlugins hiding ((<>))


staticPlugin :: DynFlags -> DynFlags
staticPlugin df
  = allowEmptyCaseButWithWarning
  $ enableQuasiQuotes
  $ df
    { staticPlugins = staticPlugins df <> [metaprogrammingPlugin] }


pattern MetaprogramSourceText :: SourceText
pattern MetaprogramSourceText = SourceText "wingman-meta-program"



pattern WingmanMetaprogram :: FastString -> HsExpr p
pattern WingmanMetaprogram mp
  <- HsSCC _ MetaprogramSourceText (StringLiteral NoSourceText mp)
      (L _ ( HsVar _ _))


enableQuasiQuotes :: DynFlags -> DynFlags
enableQuasiQuotes = flip xopt_set QuasiQuotes


-- | Wingman wants to support destructing of empty cases, but these are a parse
-- error by default. So we want to enable 'EmptyCase', but then that leads to
-- silent errors without 'Opt_WarnIncompletePatterns'.
allowEmptyCaseButWithWarning :: DynFlags -> DynFlags
allowEmptyCaseButWithWarning =
  flip xopt_set EmptyCase . flip wopt_set Opt_WarnIncompletePatterns


metaprogrammingPlugin :: StaticPlugin
metaprogrammingPlugin =
    StaticPlugin $ PluginWithArgs (defaultPlugin { parsedResultAction = worker })  []
  where
    worker :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
    worker _ _ pm = pure $ pm { hpm_module = addMetaprogrammingSyntax $ hpm_module pm }


mkMetaprogram :: SrcSpan -> FastString -> HsExpr GhcPs
mkMetaprogram ss mp =
  HsSCC noExt MetaprogramSourceText (StringLiteral NoSourceText mp)
    $ L ss
    $ HsVar noExt
    $ L ss
    $ mkRdrUnqual
    $ mkVarOcc "_"


addMetaprogrammingSyntax :: Data a => a -> a
addMetaprogrammingSyntax =
  everywhere $ mkT $ \case
    L ss (HsSpliceE _ (HsQuasiQuote _ _ (occNameString . rdrNameOcc -> "wingman") _ mp)) ->
      L ss $ mkMetaprogram ss mp
    (x :: LHsExpr GhcPs) -> x

