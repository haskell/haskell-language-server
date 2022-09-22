{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ide.Plugin.HaddockComments.Prelude where
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint (AnnKey, Annotation)

-- | A more generic comments generator
data GenComments = GenComments
  { title      :: T.Text,
    -- | Use 'Maybe' monad to exit early. 'Nothing' means a code action for haddock comments
    -- in the given context is not possible.
    updateAnns :: LHsDecl GhcPs -> TransformT Maybe ()
  }

-- | Defines how to generate haddock comments by tweaking annotations of AST
--
-- This is left here for compatibility reason, so that we don't break the existing code.
data GenCommentsSimple = forall a.
  GenCommentsSimple
  { title         :: T.Text,
    fromDecl      :: HsDecl GhcPs -> Maybe a,
    collectKeys   :: a -> [AnnKey],
    isFresh       :: Annotation -> Bool,
    updateAnn     :: Annotation -> Annotation,
    updateDeclAnn :: Annotation -> Annotation
  }
