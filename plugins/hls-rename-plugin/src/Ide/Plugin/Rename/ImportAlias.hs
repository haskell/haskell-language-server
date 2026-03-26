{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

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
fast, even when the user renames multiple aliases in quick succession.

NOTE: This module avoids manipulating LSP 'Position' and 'Range' values
directly, because by default these are in UTF-16 code units, while GHC source
spans are in Unicode code points. Instead, this module uses
'VFS.CodePointPosition' and 'VFS.CodePointRange'.
-}
module Ide.Plugin.Rename.ImportAlias
    ( getParsedModuleStale
    , ImportAlias (..)
    , findImportAliasDeclAtPos
    , findImportAliasUseAtPos
    , resolveAliasAtPos
    , aliasBasedRename
    , importAliasUseSiteSpans
    , importAliasUseSiteEdit
    , importAliasDeclEdit
    , codePointRangeContainsPosition
    ) where

import           Control.Lens                     ((^.))
import           Control.Monad.Except             (ExceptT,
                                                   MonadError (throwError))
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
import qualified Language.LSP.VFS                 as VFS

-- | The module name, alias name, and declaration span for an import alias.
-- For example, @import Data.List as L@ corresponds to
-- @ImportAlias "Data.List" "L" <span of "L">@.
data ImportAlias = ImportAlias
    { aliasModuleName :: ModuleName
    , aliasName       :: ModuleName
    , aliasDeclSpan   :: RealSrcSpan
    }

-- | Fetch the parsed module for a file, accepting a stale result.
-- Returns @Nothing@ if the file has never been indexed.
getParsedModuleStale
    :: MonadIO m
    => IdeState
    -> NormalizedFilePath
    -> m (Maybe ParsedModule)
getParsedModuleStale state nfp =
    liftIO $ fmap fst <$>
        runAction "rename.getParsedModuleStale" state
            (useWithStale GetParsedModule nfp)

-- | Find the 'ImportAlias' for the alias declaration at the cursor, such as
-- @Alias@ in @import Module as Alias@.
findImportAliasDeclAtPos
    :: VFS.CodePointPosition
    -> [LImportDecl GhcPs]
    -> Maybe ImportAlias
findImportAliasDeclAtPos codePointPos imports = listToMaybe
    [ ImportAlias {aliasModuleName, aliasName, aliasDeclSpan}
    | importDecl                  <- map unLoc imports
    , Just locatedAlias           <- [ideclAs importDecl]
    , RealSrcSpan aliasDeclSpan _ <- [getLocA locatedAlias]
    , codePointRangeContainsPosition (realSrcSpanToCodePointRange aliasDeclSpan) codePointPos
    , let aliasName       = unLoc locatedAlias
          aliasModuleName = unLoc (ideclName importDecl)
    ]

-- | Find the 'ImportAlias' matching the name qualifier at the cursor, such as
-- @Alias@ in @Alias.name@.
-- Returns multiple values if multiple modules share the same alias.
findImportAliasUseAtPos
    :: VFS.CodePointPosition
    -> [LHsDecl GhcPs]
    -> [LImportDecl GhcPs]
    -> [ImportAlias]
findImportAliasUseAtPos codePointPos decls imports =
    case listToMaybe
        [ qualifier
        | locatedRdrName :: XRec GhcPs RdrName <- listify (const True) decls
        , Qual qualifier _                     <- [unLoc locatedRdrName]
        , RealSrcSpan useSiteSpan _            <- [getLocA locatedRdrName]
        , codePointRangeContainsPosition (realSrcSpanToCodePointRange useSiteSpan) codePointPos
        , let qualifierLength = fromIntegral (length (moduleNameString qualifier))
              spanStart       = realSrcSpanStart useSiteSpan
              line            = fromIntegral (srcLocLine spanStart) - 1
              startColumn     = fromIntegral (srcLocCol  spanStart) - 1
              qualifierRange  = VFS.CodePointRange
                  (VFS.CodePointPosition line startColumn)
                  (VFS.CodePointPosition line (startColumn + qualifierLength))
        , codePointRangeContainsPosition qualifierRange codePointPos
        ] of
    Nothing -> []
    Just qualifierAtPos ->
        [ ImportAlias {aliasModuleName, aliasName, aliasDeclSpan}
        | importDecl                  <- map unLoc imports
        , Just locatedAlias           <- [ideclAs importDecl]
        , let aliasName = unLoc locatedAlias
        , aliasName == qualifierAtPos
        , RealSrcSpan aliasDeclSpan _ <- [getLocA locatedAlias]
        , let aliasModuleName = unLoc (ideclName importDecl)
        ]

-- | Return the module name and declaration span for the alias being renamed at
-- the cursor. The cursor may be on the alias token in an import declaration or
-- on a qualifier at a use site. If multiple imports share the same alias, falls
-- back to the typechecked module's 'GlobalRdrEnv' to disambiguate.
-- Returns @Nothing@ if the cursor is not on any alias declaration or qualifier.
-- HACK: The first argument is `Rename.getNamesAtPos`, parameterized to avoid a
-- circular dependency.
resolveAliasAtPos
    :: MonadIO m
    => (IdeState -> NormalizedFilePath -> Position -> ExceptT PluginError m [Name])
    -> IdeState
    -> NormalizedFilePath
    -> Position
    -> VFS.CodePointPosition
    -> [LHsDecl GhcPs]
    -> [LImportDecl GhcPs]
    -> ExceptT PluginError m (Maybe ImportAlias)
resolveAliasAtPos getNamesAtPosFn state nfp pos codePointPos decls imports =
    case findImportAliasDeclAtPos codePointPos imports of
        Just result -> pure (Just result)
        Nothing     -> case findImportAliasUseAtPos codePointPos decls imports of
            []       -> pure Nothing
            [result] -> pure (Just result)
            candidates -> do
                namesAtPos <- getNamesAtPosFn state nfp pos
                disambiguated <- disambiguateAliasUse state nfp namesAtPos candidates
                case disambiguated of
                    [] -> pure Nothing
                    [result] -> pure (Just result)
                    aliases -> throwError $ PluginInvalidParams $
                        ambiguousAliasErrorMessage aliases
    where
        ambiguousAliasErrorMessage [] = ""
        ambiguousAliasErrorMessage [_] = ""
        ambiguousAliasErrorMessage aliases@(alias1 : alias2 : _) =
            let aliasCount = T.show (length aliases)
                aliasText = T.pack (moduleNameString (aliasName alias1))
                module1 = T.pack (moduleNameString (aliasModuleName alias1))
                module2 = T.pack (moduleNameString (aliasModuleName alias2))
                quote t = "‘" <> t <> "’"
            in ("Alias " <> quote aliasText
                <> " is ambiguous (matching " <> aliasCount
                <> " imports, including "
                <> quote module1 <> " and " <> quote module2
                <> "). Try renaming " <> quote aliasText
                <> " in one of these import declarations directly.")

-- | Build a 'WorkspaceEdit' renaming an import alias and all its use sites.
aliasBasedRename
    :: MonadIO m
    => IdeState
    -> NormalizedFilePath
    -> Uri
    -> ImportAlias
    -> [LImportDecl GhcPs]
    -> [LHsDecl GhcPs]
    -> T.Text
    -> ExceptT PluginError m (MessageResult Method_TextDocumentRename)
aliasBasedRename state nfp uri importAlias imports decls newNameText = do
    let oldAlias = aliasName importAlias
        declSpan = aliasDeclSpan importAlias
        duplicateAlias =
            length [ ()
                   | importDecl <- map unLoc imports
                   , Just locatedAlias <- [ideclAs importDecl]
                   , unLoc locatedAlias == oldAlias
                   ] > 1
    virtualFile <- runActionE "rename.getVirtualFile" state $
        handleMaybeM (PluginInternalError ("Virtual file not found: " <> T.show nfp)) $
        getVirtualFile nfp
    useSiteSpans <-
        if duplicateAlias
        then importAliasUseSiteSpansDisambiguated state nfp importAlias decls
        else pure $ importAliasUseSiteSpans importAlias decls
    declEdit <- handleMaybe (PluginInternalError "Alias declaration span is out of range") $
        importAliasDeclEdit virtualFile newNameText declSpan
    useEdits <- handleMaybe (PluginInternalError "A use site span is out of range") $
        mapM (importAliasUseSiteEdit virtualFile oldAlias newNameText) useSiteSpans
    let allEdits = declEdit : useEdits
    verTxtDocId <- liftIO $ runAction "rename.getVersionedTextDoc" state $
        getVersionedTextDoc (TextDocumentIdentifier uri)
    let fileChanges = Just $ M.singleton (verTxtDocId ^. L.uri) allEdits
        -- TODO: Replace 'Nothing' with meaningful details for the workspace edit.
        workspaceEdit = WorkspaceEdit fileChanges Nothing Nothing
    pure $ InL workspaceEdit

-- | Collect the 'RealSrcSpan' of every qualified use of @oldAlias@, such as in
-- @oldAlias.foo@, @oldAlias.bar@, and so on.
-- Does not disambiguate if multiple imports share the alias.
importAliasUseSiteSpans
    :: ImportAlias
    -> [LHsDecl GhcPs]
    -> [RealSrcSpan]
importAliasUseSiteSpans importAlias decls =
    [ fullNameSpan
    | locatedRdrName :: XRec GhcPs RdrName <- listify (const True) decls
    , Qual qualifier _                     <- [unLoc locatedRdrName]
    , qualifier == aliasName importAlias
    , RealSrcSpan fullNameSpan _           <- [getLocA locatedRdrName]
    ]

-- | Build a 'TextEdit' replacing the qualifier part in a qualified name (like
-- from @Alias.name@ to @NewAlias.name@).
-- Returns 'Nothing' if the span is out of bounds in the VFS.
importAliasUseSiteEdit
    :: VFS.VirtualFile
    -> ModuleName   -- ^ old alias, used to compute the qualifier width
    -> T.Text       -- ^ new alias text
    -> RealSrcSpan  -- ^ span of the full qualified name, e.g. @Alias.name@
    -> Maybe TextEdit
importAliasUseSiteEdit virtualFile oldAlias newAlias fullNameSpan =
    codePointRangeToTextEdit virtualFile newAlias qualifierCodePointRange
    where
        spanStart           = realSrcSpanStart fullNameSpan
        line                = fromIntegral (srcLocLine spanStart) - 1
        startColumn         = fromIntegral (srcLocCol  spanStart) - 1
        qualifierLength     = fromIntegral (length (moduleNameString oldAlias))
        qualifierCodePointRange = VFS.CodePointRange
            (VFS.CodePointPosition line startColumn)
            (VFS.CodePointPosition line (startColumn + qualifierLength))

-- | Build a 'TextEdit' replacing the alias token in an import declaration (like
-- from @import Module as Alias@ to @import Module as NewAlias@).
-- Returns 'Nothing' if the span is out of bounds in the VFS.
importAliasDeclEdit
    :: VFS.VirtualFile
    -> T.Text       -- ^ new alias text
    -> RealSrcSpan  -- ^ span of @Alias@ in @import Module as Alias@
    -> Maybe TextEdit
importAliasDeclEdit virtualFile newAlias rsp =
    codePointRangeToTextEdit virtualFile newAlias (realSrcSpanToCodePointRange rsp)

-- | Check whether a 'CodePointRange' contains a 'CodePointPosition'
-- (inclusive start, exclusive end).
codePointRangeContainsPosition :: VFS.CodePointRange -> VFS.CodePointPosition -> Bool
codePointRangeContainsPosition
    (VFS.CodePointRange
        (VFS.CodePointPosition startLine startColumn)
        (VFS.CodePointPosition endLine endColumn))
    (VFS.CodePointPosition line column)
    =  (line > startLine || (line == startLine && column >= startColumn))
    && (line < endLine   || (line == endLine   && column <  endColumn))

---------------------------------------------------------------------------------------------------
-- Internal helpers

-- | Resolve an ambiguous alias use site by consulting the typechecked
-- module's 'GlobalRdrEnv'. Used when multiple imports share the same alias.
-- The caller is responsible for providing the names at the cursor.
-- Returns multiple results if they both export the same name (such as @L.view@
-- with both @Control.Lens as L@ and @Control.Lens.Getter as L@).
disambiguateAliasUse
    :: MonadIO m
    => IdeState
    -> NormalizedFilePath
    -> [Name]
    -> [ImportAlias]
    -> ExceptT PluginError m [ImportAlias]
disambiguateAliasUse state nfp namesAtPos candidates = do
    tcModule <- runActionE "rename.disambiguateAlias" state (useE TypeCheck nfp)
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)
    pure
        [ candidate
        | name <- namesAtPos
        , globalRdrEnvElement <- maybeToList (lookupGRE_Name rdrEnv name)
        , importSpec <- gre_imp globalRdrEnvElement
        , candidate@ImportAlias{aliasModuleName} <- candidates
        , importSpecModule importSpec == aliasModuleName
        ]

-- | Like 'importAliasUseSiteSpans' but filters to use sites that resolve
-- to names from @actualMod@, using the typechecked module's 'GlobalRdrEnv'.
-- Used when multiple imports share the same alias.
importAliasUseSiteSpansDisambiguated
    :: MonadIO m
    => IdeState
    -> NormalizedFilePath
    -> ImportAlias
    -> [LHsDecl GhcPs]
    -> ExceptT PluginError m [RealSrcSpan]
importAliasUseSiteSpansDisambiguated state nfp importAlias decls = do
    tcModule <- runActionE "rename.useSiteSpans" state (useE TypeCheck nfp)
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)
        ImportAlias{aliasModuleName, aliasName} = importAlias
        allSpans = importAliasUseSiteSpansWithOcc aliasName decls
        maybeMatchingSpan (occName, rsp) =
            let rdrName = Qual aliasName occName
            in listToMaybe
                [ rsp
                | gre <- pickGREs rdrName $ lookupGlobalRdrEnv rdrEnv occName
                , impSpec <- gre_imp gre
                , importSpecModule impSpec == aliasModuleName
                ]
    pure $ mapMaybe maybeMatchingSpan allSpans

-- | Like 'importAliasUseSiteSpans' but also returns the 'OccName' of each
-- use, needed for 'GlobalRdrEnv' lookup in the disambiguated path.
importAliasUseSiteSpansWithOcc
    :: ModuleName
    -> [LHsDecl GhcPs]
    -> [(OccName, RealSrcSpan)]
importAliasUseSiteSpansWithOcc oldAlias decls =
    [ (occName, rsp)
    | locatedRdrName :: XRec GhcPs RdrName <- listify (const True) decls
    , Qual qualifier occName               <- [unLoc locatedRdrName]
    , qualifier == oldAlias
    , RealSrcSpan rsp _                    <- [getLocA locatedRdrName]
    ]

---------------------------------------------------------------------------------------------------
-- Util

-- | Convert a 'RealSrcSpan' to a 'VFS.CodePointRange'.
-- GHC uses 1-based lines and columns; 'CodePointPosition' is 0-based.
realSrcSpanToCodePointRange :: RealSrcSpan -> VFS.CodePointRange
realSrcSpanToCodePointRange rsp = VFS.CodePointRange
    (VFS.CodePointPosition
        (fromIntegral (srcLocLine (realSrcSpanStart rsp)) - 1)
        (fromIntegral (srcLocCol  (realSrcSpanStart rsp)) - 1))
    (VFS.CodePointPosition
        (fromIntegral (srcLocLine (realSrcSpanEnd rsp)) - 1)
        (fromIntegral (srcLocCol  (realSrcSpanEnd rsp)) - 1))

-- | Build a 'TextEdit' from a 'VFS.CodePointRange' and replacement text.
-- Returns 'Nothing' if the range is out of bounds in the VFS.
codePointRangeToTextEdit :: VFS.VirtualFile -> T.Text -> VFS.CodePointRange -> Maybe TextEdit
codePointRangeToTextEdit virtualFile newText codePointRange =
    TextEdit <$> VFS.codePointRangeToRange virtualFile codePointRange
             <*> Just newText
