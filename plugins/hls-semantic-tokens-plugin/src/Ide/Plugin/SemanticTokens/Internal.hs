{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.SemanticTokens.Internal where

import           Control.Lens                         ((^.))
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Data                            (Data)
import           Data.Generics                        (everything, mkQ)
import           Data.List                            (sortBy)
import qualified Data.List                            as List
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       listToMaybe, mapMaybe)
import           Development.IDE                      (Action,
                                                       DocAndKindMap (DKMap),
                                                       GetBindings (GetBindings),
                                                       GetDocMap (GetDocMap),
                                                       GetHieAst (GetHieAst),
                                                       GetModIface (GetModIface),
                                                       GhcSessionDeps (GhcSessionDeps, GhcSessionDeps_),
                                                       HieAstResult (HAR, hieAst, hieModule, refMap),
                                                       IdeState,
                                                       TypeCheck (TypeCheck),
                                                       catchSrcErrors, realSpan,
                                                       useWithStaleFast)
import           Development.IDE.Core.Compile         (TcModuleResult (..),
                                                       lookupName)
import           Development.IDE.Core.Rules           (getSourceFileSource,
                                                       runAction)
import           Development.IDE.Core.Shake           (ShakeExtras (..),
                                                       getShakeExtras,
                                                       withHieDb)
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Error                     (getNormalizedFilePathE)
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
-- import Language.LSP.Protocol.Types.Common
import qualified Data.Text                            as T
import           Ide.Plugin.SemanticTokens.Query
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
-- import System.FilePath (takeExtension)
import           Control.Arrow                        ((&&&), (+++))
import           Control.Monad                        (forM, forM_)
import           Control.Monad.Trans.Class            (lift)
import           Data.ByteString                      (ByteString, unpack)
import           Data.Either                          (fromRight, rights)
import           Data.Either.Extra                    (lefts)
import           Data.Function                        (on)
import           Data.Generics                        (Typeable)
import qualified Data.HashSet                         as HashSet
import           Data.List.Extra                      (chunksOf, (!?))
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import           Data.Traversable                     (for)
import           Data.Typeable                        (cast)
import           Development.IDE                      (IdeAction, IdeState,
                                                       Priority (..), ideLogger,
                                                       logPriority, use, uses)
import           Development.IDE.Core.PluginUtils     (useWithStaleFastMT,
                                                       useWithStaleMT, usesMT)
import           Development.IDE.Core.PositionMapping (zeroMapping)
import           Development.IDE.Spans.Documentation  (mkDocMap)
import           Development.IDE.Spans.LocalBindings  (getDefiningBindings,
                                                       getLocalScope)
import           Development.IDE.Types.Exports        (ExportsMap (..),
                                                       createExportsMapHieDb)
import           Development.IDE.Types.HscEnvEq       (hscEnv)
import           GHC.Conc                             (readTVar)

-- logWith :: (MonadIO m) => IdeState -> String -> m ()
-- logWith st = liftIO . logPriority (ideLogger st) Info . T.pack . show

logWith :: (MonadIO m) => IdeState -> String -> m ()
logWith st = liftIO . print

bytestringString :: ByteString -> String
bytestringString = map (toEnum . fromEnum) . unpack

-- data TyThing
--   = AnId     Id
--   | AConLike ConLike
--   | ATyCon   TyCon       -- TyCons and classes; see Note [ATyCon for classes]
--   | ACoAxiom (CoAxiom Branched)
-- a :: IdDetails
-- a = undefined

-- | Identifier Details
--
-- The 'IdDetails' of an 'Id' give stable, and necessary,
-- information about the Id.
-- data IdDetails
--   = VanillaId

--   -- | The 'Id' for a record selector
--   | RecSelId
--     { sel_tycon   :: RecSelParent
--     , sel_naughty :: Bool       -- True <=> a "naughty" selector which can't actually exist, for example @x@ in:
--                                 --    data T = forall a. MkT { x :: a }
--     }                           -- See Note [Naughty record selectors] in GHC.Tc.TyCl

--   | DataConWorkId DataCon       -- ^ The 'Id' is for a data constructor /worker/
--   | DataConWrapId DataCon       -- ^ The 'Id' is for a data constructor /wrapper/

--                                 -- [the only reasons we need to know is so that
--                                 --  a) to support isImplicitId
--                                 --  b) when desugaring a RecordCon we can get
--                                 --     from the Id back to the data con]
--   | ClassOpId Class             -- ^ The 'Id' is a superclass selector,
--                                 -- or class operation of a class

--   | PrimOpId PrimOp Bool        -- ^ The 'Id' is for a primitive operator
--                                 -- True <=> is representation-polymorphic,
--                                 --          and hence has no binding
--                                 -- This lev-poly flag is used only in GHC.Types.Id.hasNoBinding

--   | FCallId ForeignCall         -- ^ The 'Id' is for a foreign call.
--                                 -- Type will be simple: no type families, newtypes, etc

--   | TickBoxOpId TickBoxOp       -- ^ The 'Id' is for a HPC tick box (both traditional and binary)

--   | DFunId Bool                 -- ^ A dictionary function.
--        -- Bool = True <=> the class has only one method, so may be
--        --                  implemented with a newtype, so it might be bad
--        --                  to be strict on this dictionary

--   | CoVarId    -- ^ A coercion variable
--                -- This only covers /un-lifted/ coercions, of type
--                -- (t1 ~# t2) or (t1 ~R# t2), not their lifted variants
--   | JoinId JoinArity (Maybe [CbvMark])
--         -- ^ An 'Id' for a join point taking n arguments
--         -- Note [Join points] in "GHC.Core"
--         -- Can also work as a WorkerLikeId if given `CbvMark`s.
--         -- See Note [CBV Function Ids]
--         -- The [CbvMark] is always empty (and ignored) until after Tidy.
--   | WorkerLikeId [CbvMark]
--         -- ^ An 'Id' for a worker like function, which might expect some arguments to be
--         -- passed both evaluated and tagged.
--         -- Worker like functions are create by W/W and SpecConstr and we can expect that they
--         -- aren't used unapplied.
--         -- See Note [CBV Function Ids]
--         -- See Note [Tag Inference]
--         -- The [CbvMark] is always empty (and ignored) until after Tidy for ids from the current
--         -- module.

tyThingSemantic :: TyThing -> SemanticTokenType
tyThingSemantic ty = case ty of
    AnId vid
        | isTyVar vid -> TTypeVariable
        | isRecordSelector vid -> TRecField
        | isClassOpId vid -> TClassMethod
        -- | isLocalId vid -> TPatternBind
        -- | isDFunId vid -> TClassMethod
        | otherwise -> TValBind
    AConLike con -> case con of
        RealDataCon _ -> TDataCon
        PatSynCon _   -> TPatternSyn
    ATyCon tyCon
        | isDataTyCon tyCon -> TTypeCon
        | isPrimTyCon tyCon -> TTypeCon
        | isClassTyCon tyCon -> TClass
        | isTypeSynonymTyCon tyCon -> TTypeSyn
        | isTypeFamilyTyCon tyCon -> TTypeFamily
        | otherwise -> TNothing
    ACoAxiom _ -> TNothing

-----------------------
---- the api
-----------------------

computeSemanticTokens ::  IdeState -> NormalizedFilePath -> Action (Maybe SemanticTokens)
computeSemanticTokens state nfp =
    let dbg = logWith state in
        runMaybeT $ do
    -- HAR{hieAst, refMap} <- MaybeT $ use GetHieAst nfp
    [HAR{..}] <- usesMT GetHieAst [nfp]
    -- [TcModuleResult{..}]<- usesMT TypeCheck [nfp]
    [hscEnv -> hsc]        <- usesMT (GhcSessionDeps_ True) [nfp]
    -- HAR{..} <- MaybeT $ useWithStaleFastMT GetHieAst nfp
    liftIO $ putStrLn $ "moduleName: " <> showSDocUnsafe (ppr hieModule)
    let xs = Map.toList $ getAsts hieAst
    liftIO $ putStrLn $ "hieAst size: " <> show (List.length xs)

    case xs of
        ((_,ast):_) -> do
            -- compute imported names from hieAst
            let importedNames = importedNameFromModule hieModule ast
            -- accumulate names from typechecked module
            -- km <- liftIO $ foldrM (getType hsc) (tcg_type_env tmrTypechecked) importedNames
            km <- liftIO $ foldrM (getType hsc) emptyNameEnv importedNames
            let importedModuleNameSemanticMap =  Map.fromList $ flip mapMaybe (Set.toList importedNames) $ \name -> do
                    ty <- lookupNameEnv km name
                    return (name, tyThingSemantic ty)
            liftIO $ forM (Set.toList importedNames) $ \name -> do
                    let ty = lookupNameEnv km name
                    dbg $ "imported name: "
                        <> showSDocUnsafe (ppr name)
                        <> " :: " <> showSDocUnsafe (ppr ty)
                    -- return (name, tyThingSemantic ty)
            ShakeExtras{..} <- MaybeT $ fmap Just getShakeExtras
            let originalModuleNameSemanticMap = toNameSemanticMap refMap
            let combineMap = Map.unionWith (<>) originalModuleNameSemanticMap importedModuleNameSemanticMap
            let names = identifierGetter ast

            source :: ByteString <- lift $ getSourceFileSource nfp
            let moduleAbsTks = extractSemanticTokensFromNames combineMap names
            case semanticTokenAbsoluteSemanticTokens moduleAbsTks of
                Right tokens -> do
                    liftIO $ mapM_ (\x -> mapM_ (dbg . show) x) $ recoverSemanticTokens (bytestringString source) tokens
                    pure tokens
                Left err -> do
                    liftIO $ putStrLn $ "computeSemanticTokens: " <> show err
                    MaybeT . pure $ Nothing
        _ -> MaybeT . pure  $ Nothing
    where
    getType env n nameMap
      | Nothing <- lookupNameEnv nameMap n
      = do kind <- lookupKind env n
           pure $ maybe nameMap (extendNameEnv nameMap n) kind
      | otherwise = pure nameMap
    lookupKind :: HscEnv -> Name -> IO (Maybe TyThing)
    lookupKind env = fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env


semanticTokensFull :: PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull state _ param = do
    let dbg = logWith state
    nfp <-  getNormalizedFilePathE (param ^. (L.textDocument . L.uri))
    dbg $ "semanticTokensFull: " <> show nfp
    -- source :: ByteString <- lift $ getSourceFileSource nfp
    items <- liftIO
        $ runAction "SemanticTokens.semanticTokensFull" state
        $ computeSemanticTokens state nfp
    case items of
        Nothing -> pure $ InR Null
        Just items -> do
            content <- liftIO $ readFile $ fromNormalizedFilePath nfp
            -- mapM_ (mapM_ (dbg . show)) $ recoverSemanticTokens content items
            pure $ InL items


-----------------------
---- recover tokens
-----------------------

recoverSemanticTokens :: String -> SemanticTokens -> Either Text [SemanticTokenOriginal]
recoverSemanticTokens sourceCode (SemanticTokens _ xs) = fmap (fmap $ tokenOrigin sourceCode) $ dataActualToken xs


tokenOrigin :: [Char] -> ActualToken -> SemanticTokenOriginal
tokenOrigin sourceCode (line, startChar, len, tokenType, _) =
        -- convert back to count from 1
        SemanticTokenOriginal tokenType (Loc (line+1) (startChar+1) len) name
        where tLine = lines sourceCode !? fromIntegral line
              name = maybe "no source" (take (fromIntegral len) . drop (fromIntegral startChar)) tLine


dataActualToken :: [UInt] -> Either Text [ActualToken]
dataActualToken xs = maybe decodeError (Right . fmap semanticTokenAbsoluteActualToken . absolutizeTokens)
        $ mapM fromTuple (chunksOf 5 $ map fromIntegral xs)
    where
          decodeError = Left "recoverSemanticTokenRelative: wrong token data"
          fromTuple [a, b, c, d, _] = Just $ SemanticTokenRelative a b c (fromInt $ fromIntegral d) []
          fromTuple _               = Nothing

-- span: /Users/ares/src/test/lib/SemanticTokens/Types.hs:(34,12)-(38,3)
-- type RefMap a = M.Map Identifier [(Span, IdentifierDetails a)]
computeImportedSemanticTokens :: IdeState -> [NormalizedFilePath] -> Set.Set Name -> MaybeT Action NameSemanticMap
computeImportedSemanticTokens state nfps names =
    let dbg = logWith state in do
    dbg "heelo"
    let nameList = Set.toList names
    let moduleNamePairs = [(1, nameOccName name) | name <- nameList]
    return Map.empty
