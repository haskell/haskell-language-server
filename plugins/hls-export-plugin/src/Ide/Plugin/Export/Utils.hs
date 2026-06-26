{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Ide.Plugin.Export.Utils where

import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Language.LSP.Protocol.Types

rdrNameFS :: RdrName -> FastString
rdrNameFS = occNameFS . rdrNameOcc

ieParentName :: IE GhcPs -> Maybe RdrName
ieParentName e = case e of
#if MIN_VERSION_ghc(9,9,0)
  IEVar _ (L _ wn) _           -> Just (ieWrappedRdrName wn)
  IEThingAbs _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingAll _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingWith _ (L _ wn) _ _ _ -> Just (ieWrappedRdrName wn)
#else
  IEVar _ (L _ wn)             -> Just (ieWrappedRdrName wn)
  IEThingAbs _ (L _ wn)        -> Just (ieWrappedRdrName wn)
  IEThingAll _ (L _ wn)        -> Just (ieWrappedRdrName wn)
  IEThingWith _ (L _ wn) _ _   -> Just (ieWrappedRdrName wn)
#endif
  _                            -> Nothing

-- | The listed constructors of an @IEThingWith@ (@T(C1, C2)@), or 'Nothing' otherwise.
ieThingWithChildren :: IE GhcPs -> Maybe [LIEWrappedName GhcPs]
#if MIN_VERSION_ghc(9,9,0)
ieThingWithChildren (IEThingWith _ _ _ cs _) = Just cs
#else
ieThingWithChildren (IEThingWith _ _ _ cs)   = Just cs
#endif
ieThingWithChildren _                        = Nothing

-- | The head name of an @IEThingWith@, e.g. @T@ in @T(C1, C2)@.
ieThingWithHead :: IE GhcPs -> Maybe (LIEWrappedName GhcPs)
#if MIN_VERSION_ghc(9,9,0)
ieThingWithHead (IEThingWith _ n _ _ _) = Just n
#else
ieThingWithHead (IEThingWith _ n _ _)   = Just n
#endif
ieThingWithHead _                       = Nothing

ieWrappedRdrName :: IEWrappedName GhcPs -> RdrName
ieWrappedRdrName = \case
  IEName _ (L _ rdr)    -> rdr
  IEPattern _ (L _ rdr) -> rdr
  IEType _ (L _ rdr)    -> rdr
#if MIN_VERSION_ghc(9,11,0)
  IEDefault _ (L _ rdr) -> rdr
#endif
#if MIN_VERSION_ghc(9,13,0)
  IEData _ (L _ rdr)    -> rdr
#endif

-- | True when the export item's head name is the given 'FastString'.
parentNameIs :: FastString -> IE GhcPs -> Bool
parentNameIs fs = maybe False ((== fs) . rdrNameFS) . ieParentName

-- | The 'FastString' of a located wrapped name, e.g. an @IEThingWith@ child.
lieWrappedNameFS :: LIEWrappedName GhcPs -> FastString
lieWrappedNameFS = rdrNameFS . ieWrappedRdrName . unLoc

-- | True when @n@ is listed as a child constructor of an @IEThingWith@.
isInIE :: FastString -> IE GhcPs -> Bool
isInIE n = maybe False (any ((== n) . lieWrappedNameFS)) . ieThingWithChildren

singleFileEdit :: Uri -> [TextEdit] -> WorkspaceEdit
singleFileEdit uri edits = WorkspaceEdit (Just (Map.singleton uri edits)) Nothing Nothing

mkAction :: Text -> CodeAction
mkAction title = CodeAction {..}
  where
    _title = title
    _kind = Just CodeActionKind_RefactorRewrite
    _diagnostics = Nothing
    _isPreferred = Nothing
    _disabled = Nothing
    _edit = Nothing
    _command = Nothing
    _data_ = Nothing
