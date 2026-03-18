{-# LANGUAGE DataKinds #-}

{-| Logic for renaming qualified import aliases.

For example:

> -- Before: ---------------------------
> import qualified Data.List as L
> bar = L.take
> -- After: ----------------------------
> import qualified Data.List as List
> bar = List.take

The basic approach is this:

1. Get the parsed AST and see if there is an import alias at the cursor.
2. Check whether multiple modules are imported using the same alias.
3. Rename entities throughout the AST:
    * If only one module uses the alias, perform renaming using 'RdrName' and
    the parsed AST.
    * If multiple modules use the alias, perform alias resolution and renaming
    using 'GlobalRdrEnv' and the typechecked AST.

The common case, with each alias corresponding to one module, should be very
fast, even if the user renames multiple aliases in quick succession.
-}
module Ide.Plugin.Rename.ImportAlias                                                              -- [ ] AI
    ( getParsedModuleStale                                                                        -- [ ] AI
    , findImportAliasDeclAtPos                                                                    -- [ ] AI
    , findImportAliasUseAtPos                                                                     -- [ ] AI
    , resolveAliasAtPos                                                                           -- [ ] AI
    , aliasBasedRename                                                                            -- [ ] AI
    , importAliasUseSiteSpans                                                                     -- [ ] AI
    , importAliasUseSiteEdit                                                                      -- [ ] AI
    , importAliasDeclEdit                                                                         -- [ ] AI
    , rangeContainsPosition                                                                       -- [ ] AI
    ) where                                                                                       -- [ ] AI

import           Control.Lens                     ((^.))
import           Control.Monad.Except             (ExceptT, throwError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Generics
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Text                        as T
import           Development.IDE.Core.FileStore   (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service     hiding (Log)
import           Development.IDE.Core.Shake       hiding (Log)
import           Development.IDE.GHC.Compat
import           Development.IDE.Types.Location
import           Ide.Plugin.Error
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

-- | Fetch the parsed module, accepting a stale result.                                           -- [ ] AI
-- Returns @Nothing@ if the file has never been indexed.                                          -- [ ] AI
getParsedModuleStale                                                                              -- [ ] AI
    :: MonadIO m                                                                                  -- [ ] AI
    => IdeState                                                                                   -- [ ] AI
    -> NormalizedFilePath                                                                         -- [ ] AI
    -> m (Maybe ParsedModule)                                                                     -- [ ] AI
getParsedModuleStale = undefined                                                                  -- [ ] AI

-- | Find the module name for the import alias declaration at the cursor.                         -- [ ] AI
-- The cursor must be on the @Alias@ token in @import Module as Alias@.                           -- [ ] AI
findImportAliasDeclAtPos                                                                          -- [ ] AI
    :: Position                                                                                   -- [ ] AI
    -> [LImportDecl GhcPs]                                                                        -- [ ] AI
    -> Maybe (ModuleName, RealSrcSpan)                                                            -- [ ] AI
findImportAliasDeclAtPos = undefined                                                              -- [ ] AI

-- | Find all module names for the qualifier at the cursor.                                       -- [ ] AI
-- The cursor must be on the @Alias@ part of @Alias.name@.                                        -- [ ] AI
-- Returns multiple module names if they are imported using the same alias.                       -- [ ] AI
findImportAliasUseAtPos                                                                           -- [ ] AI
    :: Position                                                                                   -- [ ] AI
    -> [LHsDecl GhcPs]                                                                            -- [ ] AI
    -> [LImportDecl GhcPs]                                                                        -- [ ] AI
    -> [(ModuleName, RealSrcSpan)]                                                                -- [ ] AI
findImportAliasUseAtPos = undefined                                                               -- [ ] AI

-- | Determine the alias being renamed at the cursor position. The cursor may be                  -- [ ] AI
-- on the alias token in an import declaration or on a qualifier at a use site.                   -- [ ] AI
-- If multiple imports share the same alias, falls back to the typechecked                        -- [ ] AI
-- module's 'GlobalRdrEnv' to disambiguate.                                                       -- [ ] AI
-- Returns 'Nothing' if the cursor is not on any alias declaration or qualifier.                  -- [ ] AI
resolveAliasAtPos                                                                                 -- [ ] AI
    :: MonadIO m                                                                                  -- [ ] AI
    => IdeState                                                                                   -- [ ] AI
    -> NormalizedFilePath                                                                         -- [ ] AI
    -> Position                                                                                   -- [ ] AI
    -> [LHsDecl GhcPs]                                                                            -- [ ] AI
    -> [LImportDecl GhcPs]                                                                        -- [ ] AI
    -> ExceptT PluginError m (Maybe (ModuleName, RealSrcSpan))                                    -- [ ] AI
resolveAliasAtPos = undefined                                                                     -- [ ] AI

-- | Build a 'WorkspaceEdit' renaming an import alias and all its use sites.                      -- [ ] AI
aliasBasedRename                                                                                  -- [ ] AI
    :: MonadIO m                                                                                  -- [ ] AI
    => IdeState                                                                                   -- [ ] AI
    -> NormalizedFilePath                                                                         -- [ ] AI
    -> Uri                                                                                        -- [ ] AI
    -> ModuleName                                                                                 -- [ ] AI
    -> RealSrcSpan                                                                                -- [ ] AI
    -> [LImportDecl GhcPs]                                                                        -- [ ] AI
    -> [LHsDecl GhcPs]                                                                            -- [ ] AI
    -> T.Text                                                                                     -- [ ] AI
    -> ExceptT PluginError m (MessageResult Method_TextDocumentRename)                            -- [ ] AI
aliasBasedRename = undefined                                                                      -- [ ] AI

-- | Collect 'RealSrcSpan's of every use of @oldAlias.name@ in the declarations.                  -- [ ] AI
-- Does not disambiguate if multiple imports share the alias.                                     -- [ ] AI
importAliasUseSiteSpans                                                                           -- [ ] AI
    :: ModuleName                                                                                 -- [ ] AI
    -> [LHsDecl GhcPs]                                                                            -- [ ] AI
    -> [RealSrcSpan]                                                                              -- [ ] AI
importAliasUseSiteSpans = undefined                                                               -- [ ] AI

-- | Build a 'TextEdit' replacing the qualifier part in a qualified name (like                    -- [ ] AI
-- @L@ in @L.foo@).                                                                               -- [ ] AI
-- The span covers only the alias, not the dot.                                                   -- [ ] AI
importAliasUseSiteEdit                                                                            -- [ ] AI
    :: ModuleName   -- ^ Old alias, used to compute the qualifier width                           -- [ ] AI
    -> T.Text       -- ^ New alias text                                                           -- [ ] AI
    -> RealSrcSpan  -- ^ Span of the full qualified name, such as @L.foo@                         -- [ ] AI
    -> TextEdit                                                                                   -- [ ] AI
importAliasUseSiteEdit = undefined                                                                -- [ ] AI

-- | Build a 'TextEdit' replacing the alias token in an import declaration (like                  -- [ ] AI
-- from @import Module as Alias@ to @import Module as NewAlias@).                                 -- [ ] AI
importAliasDeclEdit                                                                               -- [ ] AI
    :: T.Text       -- ^ New alias text                                                           -- [ ] AI
    -> RealSrcSpan  -- ^ Span of @Alias@ in @import M as Alias@                                   -- [ ] AI
    -> TextEdit                                                                                   -- [ ] AI
importAliasDeclEdit = undefined                                                                   -- [ ] AI

-- | Check whether a range contains a position (inclusive start, exclusive end).                  -- [ ] AI
rangeContainsPosition :: Range -> Position -> Bool                                                -- [ ] AI
rangeContainsPosition = undefined                                                                 -- [ ] AI
