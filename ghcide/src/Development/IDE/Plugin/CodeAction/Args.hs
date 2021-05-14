{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Development.IDE.Plugin.CodeAction.Args
  ( module Development.IDE.Plugin.CodeAction.Args,
  )
where

import           Control.Lens                                 (alaf)
import qualified Data.Aeson.Types                             as A
import           Data.Maybe                                   (fromMaybe)
import           Data.Monoid                                  (Ap (..))
import qualified Data.Text                                    as T
import           Development.IDE                              (Diagnostic,
                                                               HieAstResult,
                                                               TcModuleResult)
import           Development.IDE.GHC.Compat                   (DynFlags,
                                                               ParsedModule,
                                                               ParsedSource)
import           Development.IDE.Plugin.CodeAction.ExactPrint (Rewrite,
                                                               rewriteToEdit)
import           Development.IDE.Plugin.TypeLenses            (GlobalBindingTypeSigsResult)
import           Development.IDE.Spans.LocalBindings          (Bindings)
import           Development.IDE.Types.Exports                (ExportsMap)
import           Development.IDE.Types.Options                (IdeOptions)
import           GHC.TypeLits                                 (KnownSymbol,
                                                               symbolVal)
import           Ide.Plugin.Properties
import           Language.LSP.Types                           (CodeActionKind (CodeActionQuickFix),
                                                               TextEdit)
import           Retrie                                       (Annotated (astA))
import           Retrie.ExactPrint                            (annsA)
-------------------------------------------------------------------------------------------------

type CodeActionTitle = T.Text

type CodeActionPreferred = Bool

-- | A compact representation of 'Language.LSP.Types.CodeAction's
type GhcideCodeActions = [(CodeActionTitle, Maybe CodeActionKind, Maybe CodeActionPreferred, [TextEdit])]

-------------------------------------------------------------------------------------------------
data GhcideCodeActionFunction = forall a. ToCodeAction a => GhcideCodeActionFunction String a

data GhcideCodeActionDef (r :: [PropertyKey]) = GhcideCodeActionDef (Properties r) [GhcideCodeActionFunction]

emptyGhcideCodeActionDef :: GhcideCodeActionDef '[]
emptyGhcideCodeActionDef = GhcideCodeActionDef emptyProperties []

defCodeAction :: forall a s r. (ToCodeAction a, KnownSymbol s, NotElem s r) => KeyNameProxy s -> T.Text -> a -> GhcideCodeActionDef r -> GhcideCodeActionDef ('PropertyKey s 'TBoolean : r)
defCodeAction k desc f (GhcideCodeActionDef p fx) = GhcideCodeActionDef (defineBooleanProperty k ("Code action: " <> desc) True p) (fx ++ [GhcideCodeActionFunction (symbolVal k) f])

runCodeActionDef :: GhcideCodeActionDef r -> A.Object -> CodeActionArgs -> GhcideCodeActions
runCodeActionDef (GhcideCodeActionDef _ xs) config caa = concat [toCodeAction caa f | GhcideCodeActionFunction name f <- xs, fromMaybe True $ A.parseMaybe (A..: T.pack name) config]

properties :: GhcideCodeActionDef r -> Properties r
properties (GhcideCodeActionDef p _) = p

usePropertyCodeActionDef ::
  (HasProperty s k t r) =>
  KeyNameProxy s ->
  GhcideCodeActionDef r ->
  A.Object ->
  ToHsType t
usePropertyCodeActionDef k d = useProperty k (properties d)
-------------------------------------------------------------------------------------------------

class ToTextEdit a where
  toTextEdit :: CodeActionArgs -> a -> [TextEdit]

instance ToTextEdit TextEdit where
  toTextEdit _ = pure

instance ToTextEdit Rewrite where
  toTextEdit CodeActionArgs {..} rw
    | Just df <- caaDf,
      Just ps <- caaAnnSource,
      Right x <- rewriteToEdit df (annsA ps) rw =
      x
    | otherwise = []

instance ToTextEdit a => ToTextEdit [a] where
  toTextEdit caa = foldMap (toTextEdit caa)

instance ToTextEdit a => ToTextEdit (Maybe a) where
  toTextEdit caa = maybe [] (toTextEdit caa)

instance (ToTextEdit a, ToTextEdit b) => ToTextEdit (Either a b) where
  toTextEdit caa = either (toTextEdit caa) (toTextEdit caa)

data CodeActionArgs = CodeActionArgs
  { caaExportsMap   :: ExportsMap,
    caaIdeOptions   :: IdeOptions,
    caaParsedModule :: Maybe ParsedModule,
    caaContents     :: Maybe T.Text,
    caaDf           :: Maybe DynFlags,
    caaAnnSource    :: Maybe (Annotated ParsedSource),
    caaTmr          :: Maybe TcModuleResult,
    caaHar          :: Maybe HieAstResult,
    caaBindings     :: Maybe Bindings,
    caaGblSigs      :: Maybe GlobalBindingTypeSigsResult,
    caaDiagnostics  :: Diagnostic
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

-------------------------------------------------------------------------------------------------

-- | Given 'CodeActionArgs', @a@ can be converted into the representation of code actions.
-- This class is designed to package functions that produce code actions in "Development.IDE.Plugin.CodeAction".
--
-- For each field @fld@ of 'CodeActionArgs', we make
--
-- @@
-- instance ToCodeAction r => ToCodeAction (fld -> r)
-- @@
--
-- where we take the value of @fld@ from 'CodeActionArgs' and then feed it into @(fld -> r)@.
-- If @fld@ is @Maybe a@, we make
--
-- @@
-- instance ToCodeAction r => ToCodeAction (Maybe a -> r)
-- instance ToCodeAction r => ToCodeAction (a -> r)
-- @@
class ToCodeAction a where
  toCodeAction :: CodeActionArgs -> a -> GhcideCodeActions

instance ToCodeAction a => ToCodeAction [a] where
  toCodeAction caa = foldMap (toCodeAction caa)

instance ToCodeAction a => ToCodeAction (Maybe a) where
  toCodeAction caa = maybe [] (toCodeAction caa)

instance ToTextEdit a => ToCodeAction (CodeActionTitle, a) where
  toCodeAction caa (title, te) = [(title, Just CodeActionQuickFix, Nothing, toTextEdit caa te)]

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionKind, a) where
  toCodeAction caa (title, kind, te) = [(title, Just kind, Nothing, toTextEdit caa te)]

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionPreferred, a) where
  toCodeAction caa (title, isPreferred, te) = [(title, Just CodeActionQuickFix, Just isPreferred, toTextEdit caa te)]

instance ToTextEdit a => ToCodeAction (CodeActionTitle, CodeActionKind, CodeActionPreferred, a) where
  toCodeAction caa (title, kind, isPreferred, te) = [(title, Just kind, Just isPreferred, toTextEdit caa te)]

-------------------------------------------------------------------------------------------------

-- | Complement: we can obtain 'ParsedSource' from 'caaAnnSource'
instance ToCodeAction r => ToCodeAction (ParsedSource -> r) where
  toCodeAction caa@CodeActionArgs {caaAnnSource = Just ps} f = toCodeAction caa $ f $ astA ps
  toCodeAction _ _ = []

-- The following boilerplate code can be generated by 'mkInstances'.
-- Now it was commented out with generated code spliced out,
-- because fields of 'CodeActionArgs' don't change frequently.
--
-- mkInstances :: Name -> DecsQ
-- mkInstances tyConName =
--   reify tyConName >>= \case
--     (TyConI (DataD _ _ _ _ [RecC dataConName tys] _)) -> concat <$> mapM (genForVar dataConName) tys
--     _ -> error "unsupported"
--  where
--   clsType = conT $ mkName "ToCodeAction"
--   methodName = mkName "toCodeAction"
--   tempType = varT $ mkName "r"
--   commonFun dataConName fieldName =
--     funD
--       methodName
--       [ clause
--           [ mkName "caa"
--               `asP` recP
--                 dataConName
--                 [fieldPat fieldName $ varP (mkName "x")]
--           , varP (mkName "f")
--           ]
--           (normalB [|$(varE methodName) caa $ f x|])
--           []
--       ]
--   genForVar dataConName (fieldName, _, ty@(AppT (ConT _maybe) ty'))
--     | _maybe == ''Maybe =
--       do
--         withMaybe <-
--           instanceD
--             (cxt [clsType `appT` tempType])
--             (clsType `appT` ((arrowT `appT` pure ty) `appT` tempType))
--             [commonFun dataConName fieldName]
--         withoutMaybe <-
--           instanceD
--             (cxt [clsType `appT` tempType])
--             (clsType `appT` ((arrowT `appT` pure ty') `appT` tempType))
--             [ funD
--                 methodName
--                 [ clause
--                     [ mkName "caa"
--                         `asP` recP
--                           dataConName
--                           [fieldPat fieldName $ conP 'Just [varP (mkName "x")]]
--                     , varP (mkName "f")
--                     ]
--                     (normalB [|$(varE methodName) caa $ f x|])
--                     []
--                 , clause [wildP, wildP] (normalB [|[]|]) []
--                 ]
--             ]
--         pure [withMaybe, withoutMaybe]
--   genForVar dataConName (fieldName, _, ty) =
--     pure
--       <$> instanceD
--         (cxt [clsType `appT` tempType])
--         (clsType `appT` ((arrowT `appT` pure ty) `appT` tempType))
--         [commonFun dataConName fieldName]

instance ToCodeAction r => ToCodeAction (ExportsMap -> r) where
  toCodeAction caa@CodeActionArgs {caaExportsMap = x} f =
    toCodeAction caa $ f x

instance ToCodeAction r => ToCodeAction (IdeOptions -> r) where
  toCodeAction caa@CodeActionArgs {caaIdeOptions = x} f =
    toCodeAction caa $ f x

instance
  ToCodeAction r =>
  ToCodeAction (Maybe ParsedModule -> r)
  where
  toCodeAction caa@CodeActionArgs {caaParsedModule = x} f =
    toCodeAction caa $ f x

instance ToCodeAction r => ToCodeAction (ParsedModule -> r) where
  toCodeAction caa@CodeActionArgs {caaParsedModule = Just x} f =
    toCodeAction caa $ f x
  toCodeAction _ _ = []

instance ToCodeAction r => ToCodeAction (Maybe T.Text -> r) where
  toCodeAction caa@CodeActionArgs {caaContents = x} f =
    toCodeAction caa $ f x

instance ToCodeAction r => ToCodeAction (T.Text -> r) where
  toCodeAction caa@CodeActionArgs {caaContents = Just x} f =
    toCodeAction caa $ f x
  toCodeAction _ _ = []

instance ToCodeAction r => ToCodeAction (Maybe DynFlags -> r) where
  toCodeAction caa@CodeActionArgs {caaDf = x} f =
    toCodeAction caa $ f x

instance ToCodeAction r => ToCodeAction (DynFlags -> r) where
  toCodeAction caa@CodeActionArgs {caaDf = Just x} f =
    toCodeAction caa $ f x
  toCodeAction _ _ = []

instance
  ToCodeAction r =>
  ToCodeAction (Maybe (Annotated ParsedSource) -> r)
  where
  toCodeAction caa@CodeActionArgs {caaAnnSource = x} f =
    toCodeAction caa $ f x

instance
  ToCodeAction r =>
  ToCodeAction (Annotated ParsedSource -> r)
  where
  toCodeAction caa@CodeActionArgs {caaAnnSource = Just x} f =
    toCodeAction caa $ f x
  toCodeAction _ _ = []

instance
  ToCodeAction r =>
  ToCodeAction (Maybe TcModuleResult -> r)
  where
  toCodeAction caa@CodeActionArgs {caaTmr = x} f =
    toCodeAction caa $ f x

instance ToCodeAction r => ToCodeAction (TcModuleResult -> r) where
  toCodeAction caa@CodeActionArgs {caaTmr = Just x} f =
    toCodeAction caa $ f x
  toCodeAction _ _ = []

instance
  ToCodeAction r =>
  ToCodeAction (Maybe HieAstResult -> r)
  where
  toCodeAction caa@CodeActionArgs {caaHar = x} f =
    toCodeAction caa $ f x

instance ToCodeAction r => ToCodeAction (HieAstResult -> r) where
  toCodeAction caa@CodeActionArgs {caaHar = Just x} f =
    toCodeAction caa $ f x
  toCodeAction _ _ = []

instance ToCodeAction r => ToCodeAction (Maybe Bindings -> r) where
  toCodeAction caa@CodeActionArgs {caaBindings = x} f =
    toCodeAction caa $ f x

instance ToCodeAction r => ToCodeAction (Bindings -> r) where
  toCodeAction caa@CodeActionArgs {caaBindings = Just x} f =
    toCodeAction caa $ f x
  toCodeAction _ _ = []

instance
  ToCodeAction r =>
  ToCodeAction (Maybe GlobalBindingTypeSigsResult -> r)
  where
  toCodeAction caa@CodeActionArgs {caaGblSigs = x} f =
    toCodeAction caa $ f x

instance
  ToCodeAction r =>
  ToCodeAction (GlobalBindingTypeSigsResult -> r)
  where
  toCodeAction caa@CodeActionArgs {caaGblSigs = Just x} f =
    toCodeAction caa $ f x
  toCodeAction _ _ = []

instance ToCodeAction r => ToCodeAction (Diagnostic -> r) where
  toCodeAction caa@CodeActionArgs {caaDiagnostics = x} f =
    toCodeAction caa $ f x

-------------------------------------------------------------------------------------------------
