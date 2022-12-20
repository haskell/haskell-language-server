module Development.IDE.Plugin.Plugins.ImportUtils
  ( ImportStyle(..),
    quickFixImportKind',
    quickFixImportKind,
    renderImportStyle,
    unImportStyle,
    importStyles
  ) where

import           Data.List.NonEmpty                           (NonEmpty ((:|)))
import qualified Data.Text                                    as T
import           Development.IDE.Plugin.CodeAction.ExactPrint (wildCardSymbol)
import           Development.IDE.Types.Exports
import           Language.LSP.Types                           (CodeActionKind (..))

-- | Possible import styles for an 'IdentInfo'.
--
-- The first 'Text' parameter corresponds to the 'rendered' field of the
-- 'IdentInfo'.
data ImportStyle
    = ImportTopLevel T.Text
      -- ^ Import a top-level export from a module, e.g., a function, a type, a
      -- class.
      --
      -- > import M (?)
      --
      -- Some exports that have a parent, like a type-class method or an
      -- associated type/data family, can still be imported as a top-level
      -- import.
      --
      -- Note that this is not the case for constructors, they must always be
      -- imported as part of their parent data type.

    | ImportViaParent T.Text T.Text
      -- ^ Import an export (first parameter) through its parent (second
      -- parameter).
      --
      -- import M (P(?))
      --
      -- @P@ and @?@ can be a data type and a constructor, a class and a method,
      -- a class and an associated type/data family, etc.

    | ImportAllConstructors T.Text
      -- ^ Import all constructors for a specific data type.
      --
      -- import M (P(..))
      --
      -- @P@ can be a data type or a class.
  deriving Show

importStyles :: IdentInfo -> NonEmpty ImportStyle
importStyles i@(IdentInfo {parent})
  | Just p <- pr
    -- Constructors always have to be imported via their parent data type, but
    -- methods and associated type/data families can also be imported as
    -- top-level exports.
  = ImportViaParent rend p
      :| [ImportTopLevel rend | not (isDatacon i)]
      <> [ImportAllConstructors p]
  | otherwise
  = ImportTopLevel rend :| []
  where rend = rendered i
        pr = occNameText <$> parent

-- | Used for adding new imports
renderImportStyle :: ImportStyle -> T.Text
renderImportStyle (ImportTopLevel x)   = x
renderImportStyle (ImportViaParent x p@(T.uncons -> Just ('(', _))) = "type " <> p <> "(" <> x <> ")"
renderImportStyle (ImportViaParent x p) = p <> "(" <> x <> ")"
renderImportStyle (ImportAllConstructors p) = p <> "(..)"

-- | Used for extending import lists
unImportStyle :: ImportStyle -> (Maybe String, String)
unImportStyle (ImportTopLevel x)        = (Nothing, T.unpack x)
unImportStyle (ImportViaParent x y)     = (Just $ T.unpack y, T.unpack x)
unImportStyle (ImportAllConstructors x) = (Just $ T.unpack x, wildCardSymbol)


quickFixImportKind' :: T.Text -> ImportStyle -> CodeActionKind
quickFixImportKind' x (ImportTopLevel _) = CodeActionUnknown $ "quickfix.import." <> x <> ".list.topLevel"
quickFixImportKind' x (ImportViaParent _ _) = CodeActionUnknown $ "quickfix.import." <> x <> ".list.withParent"
quickFixImportKind' x (ImportAllConstructors _) = CodeActionUnknown $ "quickfix.import." <> x <> ".list.allConstructors"

quickFixImportKind :: T.Text -> CodeActionKind
quickFixImportKind x = CodeActionUnknown $ "quickfix.import." <> x
