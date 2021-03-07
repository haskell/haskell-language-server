{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Development.IDE.Plugin.CodeAction.Args (
  module Development.IDE.Plugin.CodeAction.Args,
) where

import           Control.Lens                                 (alaf)
import           Data.Bifunctor                               (second)
import           Data.Monoid                                  (Ap (..))
import qualified Data.Text                                    as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           Development.IDE.Plugin.CodeAction.Args.TH
import           Development.IDE.Plugin.CodeAction.ExactPrint
import           Development.IDE.Plugin.TypeLenses            (GlobalBindingTypeSigsResult)
import           Development.IDE.Spans.LocalBindings          (Bindings)
import           Development.IDE.Types.Exports                (ExportsMap)
import           Development.IDE.Types.Options                (IdeOptions)
import           Language.LSP.Types                           (TextEdit,
                                                               type (|?) (..))
import           Retrie                                       (Annotated (astA))
import           Retrie.ExactPrint                            (annsA)

data CodeActionArgs = CodeActionArgs
  { caaExportsMap   :: ExportsMap
  , caaIdeOptions   :: IdeOptions
  , caaParsedModule :: Maybe ParsedModule
  , caaContents     :: Maybe T.Text
  , caaDf           :: Maybe DynFlags
  , caaAnnSource    :: Maybe (Annotated ParsedSource)
  , caaTmr          :: Maybe TcModuleResult
  , caaHar          :: Maybe HieAstResult
  , caaBindings     :: Maybe Bindings
  , caaGblSigs      :: Maybe GlobalBindingTypeSigsResult
  , caaDiagnostics  :: Diagnostic
  }

rewrite ::
  Maybe DynFlags ->
  Maybe (Annotated ParsedSource) ->
  [(T.Text, [Rewrite])] ->
  [(T.Text, [TextEdit])]
rewrite (Just df) (Just ps) r
  | Right edit <-
      (traverse . traverse)
        (alaf Ap foldMap (rewriteToEdit df (annsA ps)))
        r =
    edit
rewrite _ _ _ = []

-- we need this intermediate existential type to encapsulate functions producing code actions into a list
data SomeAction = forall a. ToCodeAction a => SomeAction a

wrap :: ToCodeAction a => a -> SomeAction
wrap = SomeAction

unwrap :: CodeActionArgs -> SomeAction -> [(T.Text, [TextEdit])]
unwrap caa (SomeAction x) = toCodeAction caa x

class ToCodeAction a where
  toCodeAction :: CodeActionArgs -> a -> [(T.Text, [TextEdit])]

instance ToCodeAction [(T.Text, [TextEdit])] where
  toCodeAction _ = id

instance ToCodeAction [(T.Text, [Rewrite])] where
  toCodeAction CodeActionArgs{..} = rewrite caaDf caaAnnSource

instance ToCodeAction r => ToCodeAction (ParsedSource -> r) where
  toCodeAction caa@CodeActionArgs{caaAnnSource = Just ps} f = toCodeAction caa $ f $ astA ps
  toCodeAction _ _ = []

instance ToCodeAction [(T.Text, [TextEdit |? Rewrite])] where
  toCodeAction CodeActionArgs{..} r = second (concatMap go) <$> r
   where
    go (InL te) = [te]
    go (InR rw)
      | Just df <- caaDf
        , Just ps <- caaAnnSource
        , Right x <- rewriteToEdit df (annsA ps) rw =
        x
      | otherwise = []

-- generates instances of 'ToCodeAction',
-- where the pattern is @instance ToCodeAction r => ToCodeAction (field -> r)@, for each field of 'CodeActionArgs'.
-- therefore functions to produce code actions in CodeAction.hs can be wrapped into 'SomeAction' without modification.
-- for types applied to 'Maybe', it generates to instances: for example,
--
-- @
-- instance ToCodeAction r => ToCodeAction (Maybe DynFlags -> r) where
--   toCodeAction caa@CodeActionArgs {caaDf = x} f = toCodeAction caa $ f x
-- @
--
-- and
--
-- @
-- instance ToCodeAction r => ToCodeAction (DynFlags -> r) where
--   toCodeAction caa@CodeActionArgs {caaDf = Just x} f = toCodeAction caa $ f x
--   toCodeAction _ _  = []
-- @
-- will be derived from 'caaDf'.
mkInstances ''CodeActionArgs
