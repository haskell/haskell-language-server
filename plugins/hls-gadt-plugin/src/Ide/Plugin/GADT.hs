{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.GADT (descriptor) where

import           Control.Lens                            ((^.))
import           Control.Monad.Except
import           Data.Aeson                              (FromJSON, ToJSON,
                                                          Value (Null), toJSON)
import           Data.Either.Extra                       (maybeToEither)
import           Data.Functor                            ((<&>))
import qualified Data.HashMap.Lazy                       as HashMap
import           Data.List.Extra                         (stripInfix)
import qualified Data.Text                               as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           GHC.Data.Maybe
import           GHC.Generics                            (Generic)
import           GHC.Parser.Annotation                   (AddEpAnn (..),
                                                          Anchor (Anchor),
                                                          AnchorOperation (MovedAnchor),
                                                          DeltaPos (..),
                                                          EpAnn (..),
                                                          EpAnnComments (EpaComments),
                                                          EpaLocation (EpaDelta),
                                                          SrcSpanAnn' (SrcSpanAnn),
                                                          spanAsAnchor)
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint         (showAst)
import           Language.Haskell.GHC.ExactPrint.Parsers (parseDecl)
import           Language.LSP.Server                     (sendRequest)
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens                 as L

-- TODO: Add GADTs pragma
--       Constraints

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { Ide.Types.pluginHandlers =
        mkPluginHandler STextDocumentCodeAction codeActionHandler
    , pluginCommands =
        [PluginCommand toGADTSyntaxCommandId "convert data decl to GADT syntax" toGADTCommand]
    }

-- | Parameter used in the command
data ToGADTParams = ToGADTParams
    { uri   :: Uri
    , range :: Range
    } deriving (Generic, ToJSON, FromJSON)

toGADTSyntaxCommandId :: CommandId
toGADTSyntaxCommandId = "GADT.toGADT"

type GP = GhcPass Parsed

-- | A command replaces H98 data decl with GADT decl in place
toGADTCommand :: CommandFunction IdeState ToGADTParams
toGADTCommand state ToGADTParams{..} = response $ do
    nfp <- handleMaybe
        ("Unable to convert " <> show uri <> " to NormalizedFilePath")
        $ uriToNormalizedFilePath $ toNormalizedUri uri
    decls <- getInRangeH98Decls state range nfp
    (L ann decl) <- handleMaybe
        ("Expect 1 decl, but got " <> show (Prelude.length decls))
        (if Prelude.length decls == 1 then Just $ head decls else Nothing)
    deps <- liftIO $ runAction "GADT.GhcSessionDeps" state $ use GhcSessionDeps nfp
    (hsc_dflags . hscEnv -> df) <- ExceptT
        $ pure
        $ maybeToEither "Get GhcSessionDeps failed" deps
    txt <- ExceptT $ pure $ T.pack <$> (prettyGADTDecl df . h98ToGADTDecl) decl
    range <- ExceptT
        $ pure
        $ maybeToEither "Unable to get data decl range"
        $ srcSpanToRange $ locA ann
    _ <- lift $ sendRequest
            SWorkspaceApplyEdit
            (ApplyWorkspaceEditParams Nothing (workSpaceEdit nfp txt range))
            (\_ -> pure ())
    pure Null
    where
        workSpaceEdit nfp txt range = WorkspaceEdit
            (pure $ HashMap.fromList
                [(filePathToUri $ fromNormalizedFilePath nfp,
                 List [TextEdit range txt])])
                 Nothing Nothing

codeActionHandler :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionHandler state plId (CodeActionParams _ _ doc range _) = response $ do
    nfp <- getNormalizedFilePath plId doc
    inRangeH98Decls <- getInRangeH98Decls state range nfp
    let actions = map (mkAction . printOutputable . tcdLName . unLoc) inRangeH98Decls
    pure $ List actions
    where
        mkAction :: T.Text -> Command |? CodeAction
        mkAction name = InR CodeAction{..}
            where
                _title = "Convert " <> name <> " to GADT syntax"
                _kind = Just CodeActionRefactorRewrite
                _diagnostics = Nothing
                _isPreferred = Nothing
                _disabled = Nothing
                _edit = Nothing
                _command = Just
                    $ mkLspCommand plId toGADTSyntaxCommandId _title (Just [toJSON mkParam])
                _xdata = Nothing

        mkParam = ToGADTParams (doc ^. L.uri) range

getInRangeH98Decls :: (Monad (t IO), MonadTrans t) =>
    IdeState
    -> Range
    -> NormalizedFilePath
    -> ExceptT String (t IO) [LTyClDecl GP]
getInRangeH98Decls state range nfp = do
    pm <- handleMaybeM "Unable to get ParsedModuleWithComments"
        $ lift
        $ runAction "GADT.GetParsedModuleWithComments" state
        $ use GetParsedModuleWithComments nfp
    let (L _ hsDecls) = hsmodDecls <$> pm_parsed_source pm
    pure $ filter isH98DataDecl $ mapMaybe getDataDecl $ filter (inRange range) hsDecls

-- | Get data decl and its location (stored in ann)
getDataDecl :: LHsDecl GP -> Maybe (LTyClDecl GP)
getDataDecl (L l (TyClD _ d@DataDecl{})) = Just (L l d)
getDataDecl _                            = Nothing

isConDeclH98 :: ConDecl GP -> Bool
isConDeclH98 ConDeclH98{} = True
isConDeclH98 _            = False

isH98DataDecl :: LTyClDecl GP -> Bool
isH98DataDecl (L _ decl@DataDecl{}) =
    any (isConDeclH98 . (unXRec @GP)) (dd_cons $ tcdDataDefn decl)
isH98DataDecl _ = False

-- | Check if a located item is in the given range
inRange :: Range -> GenLocated (SrcSpanAnn' a) e -> Bool
inRange range (L s _) = maybe False (subRange range) (srcSpanToRange (locA s))

{- |
We use `printOutputable` to print H98 data decl as GADT syntax,
this print is not perfect, it will:

1. Make data name and the `where` key word in different lines.
2. Make the whole data decl prints in one line if there is only one data constructor.
3. The ident size of every data constructor depends on its origin
   format, and may have different ident size between constructors.

Hence, we first use `printOutputable` to get an initial GADT syntax,
then use `ghc-exactprint` to parse the initial result, and finally
adjust the details that mentioned above.

The adjustment includes:

1. Make the `where` key word at the same line of data name.
2. Remove the extra blank line caused by adjustment of `where`.
3. Make every data constructor start with a new line and 2 spaces
-}
prettyGADTDecl :: DynFlags -> TyClDecl GP -> Either String String
prettyGADTDecl df decl =
    let old = printOutputable decl
        hsDecl = parseDecl df "unused" (T.unpack old)
        tycld = adjustTyClD hsDecl
    in removeExtraEmptyLine . exactPrint <$> tycld
    where
        adjustTyClD = \case
                Right (L _ (TyClD _ tycld)) -> Right $ adjustDataDecl tycld
                Right x -> Left $ "Expect TyClD but got " <> showAst x
                Left err -> Left $ show err

        adjustDataDecl d@DataDecl{..} = d
            { tcdDExt = adjustWhere tcdDExt
            , tcdDataDefn = tcdDataDefn
                { dd_cons = map adjustCon (dd_cons tcdDataDefn)
                }
            }
        adjustDataDecl x = x

        -- Make every data constructor start with a new line and 2 spaces
        adjustCon :: LConDecl GP -> LConDecl GP
        adjustCon (L (SrcSpanAnn _ loc) r) =
            L (SrcSpanAnn (EpAnn (go (spanAsAnchor loc)) (AnnListItem []) (EpaComments [])) loc) r
            where
                go (Anchor a _) = Anchor a (MovedAnchor (DifferentLine 1 2))

        -- Adjust where annotation to the same line of the type constuctor
        adjustWhere tcdDExt = tcdDExt <&> map
            (\(AddEpAnn ann l) ->
            if ann == AnnWhere
                then AddEpAnn AnnWhere (EpaDelta (SameLine 1) [])
                else AddEpAnn ann l
            )

        -- Remove the first extra line if exist
        removeExtraEmptyLine s = case stripInfix "\n\n" s of
            Just (x, xs) -> x <> "\n" <> xs
            Nothing      -> s

-- | Convert H98 data type definition to GADT's
h98ToGADTDecl :: TyClDecl GP -> TyClDecl GP
h98ToGADTDecl = \case
    d@DataDecl{..} -> d
        { tcdDataDefn = updateDefn tcdLName tcdTyVars tcdDataDefn
        }
    x -> x
    where
        updateDefn dataName tyVars = \case
            d@HsDataDefn{..} -> d
                { dd_cons = map (mapXRec @GP (h98ToGADTConDecl dataName tyVars)) dd_cons
                }

-- | Convert H98 data constuctor to GADT data constructor
h98ToGADTConDecl ::
    LIdP GP -- ^Type constuctor name,
            -- used for constucting final result type in GADT
    -> LHsQTyVars GP
            -- ^Type variable names
            -- used for constucting final result type in GADT
    -> ConDecl GP
    -> ConDecl GP
h98ToGADTConDecl dataName tyVars = \case
    ConDeclH98{..} ->
        ConDeclGADT
            con_ext
            [con_name]
            -- Ignore all existential type variable since GADT not needed
            (wrapXRec @GP mkHsOuterImplicit)
            con_mb_cxt
            (renderDetails con_args)
            renderResultTy
            con_doc
    x -> x
    where
        -- | Convert `HsConDeclH98Details` to `HsConDeclGADTDetails`
        --
        -- Parameters in the data constructor
        renderDetails :: HsConDeclH98Details GP -> HsConDeclGADTDetails GP
        renderDetails (PrefixCon _ args)   = PrefixConGADT args
        renderDetails (InfixCon arg1 arg2) = PrefixConGADT [arg1, arg2]
        renderDetails (RecCon recs)        = RecConGADT recs

        -- | Construct GADT result type
        renderResultTy :: LHsType GP
        renderResultTy = case tyVars of
            -- Without type variable
            HsQTvs _ []   -> wrappedDatanName
            -- With type variable
            HsQTvs _ vars -> foldl go wrappedDatanName vars
            where
                wrappedDatanName = wrapXRec @GP (HsTyVar EpAnnNotUsed NotPromoted dataName)
                -- Bundle data name with type vars by `HsAppTy`
                go acc (L _(UserTyVar _ _ var)) =
                    wrapXRec @GP
                        (HsAppTy NoExtField acc
                            (wrapXRec @GP (HsTyVar EpAnnNotUsed NotPromoted var)))
                go acc _ = acc
