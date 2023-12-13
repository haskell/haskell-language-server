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
import           Debug.Trace                          (trace)
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

logWith :: (MonadIO m) => IdeState -> String -> m ()
logWith st = liftIO . logPriority (ideLogger st) Info . T.pack


-- logWith :: (MonadIO m) => IdeState -> String -> m ()
-- logWith st = liftIO . print

bytestringString :: ByteString -> String
bytestringString = map (toEnum . fromEnum) . unpack

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
        | isTypeSynonymTyCon tyCon -> TTypeSyn
        | isTypeFamilyTyCon tyCon -> TTypeFamily
        | isClassTyCon tyCon -> TClass
        | isDataTyCon tyCon -> TTypeCon
        | isPrimTyCon tyCon -> TTypeCon
        | otherwise -> TNothing
    ACoAxiom _ -> TNothing

-----------------------
---- the api
-----------------------

computeSemanticTokens ::  IdeState -> NormalizedFilePath -> Action (Maybe SemanticTokens)
computeSemanticTokens state nfp =
    runMaybeT $ do
    -- let dbg = logWith state
    -- let getAst HAR{hieAst, refMap} = hieAst
    (HAR{hieAst, refMap, hieModule}, _) <- useWithStaleMT GetHieAst nfp
    (_, ast) <- MaybeT $ return $ listToMaybe $ Map.toList $ getAsts hieAst
    (TcModuleResult{..}, _) <- useWithStaleMT TypeCheck nfp
    (hscEnv -> hsc, _) <- useWithStaleMT GhcSessionDeps nfp
    -- because the names get from ast might contain derived name
    let nameSet = nameGetter tmrRenamed
    -- let nameSet = hieAstNameSet ast
    let names = hieAstSpanNames ast

    -- ask hscEnv for none local types
    km <- liftIO $ foldrM (getType hsc) (tcg_type_env tmrTypechecked) nameSet
    -- name from type typecheck
    let importedModuleNameSemanticMap =  Map.fromList $ flip mapMaybe (Set.toList nameSet) $ \name -> do
            ty <- lookupNameEnv km name
            return (name, tyThingSemantic ty)
    let localNameSemanticMap = toNameSemanticMap $ Map.filterWithKey (\k _ ->
                either (const False) (flip Set.member nameSet) k) refMap
    let combineMap = Map.unionWith (<>) localNameSemanticMap importedModuleNameSemanticMap
    -- print all names
    -- deriving method locate in the same position as the class name
    -- liftIO $ mapM_ (\ (name, tokenType) -> dbg ("debug semanticMap: " <> showClearName name <> ":" <> show tokenType )) $ Map.toList importedModuleNameSemanticMap
    -- liftIO $ mapM_ (\ (span, name) -> dbg ("debug names: " <> showClearName name <> ":" <> printCompactRealSrc span ) ) names
    let moduleAbsTks = extractSemanticTokensFromNames combineMap names
    case semanticTokenAbsoluteSemanticTokens moduleAbsTks of
        Right tokens -> do
            source :: ByteString <- lift $ getSourceFileSource nfp
            -- liftIO $ mapM_ (\x -> mapM_ (dbg . show) x) $ recoverSemanticTokens (bytestringString source) tokens
            pure tokens
        Left err -> do
            liftIO $ putStrLn $ "computeSemanticTokens: " <> show err
            MaybeT . pure $ Nothing
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
    -- let dbg = logWith state
    nfp <-  getNormalizedFilePathE (param ^. (L.textDocument . L.uri))
    -- dbg $ "semanticTokensFull: " <> show nfp
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

-- | recoverSemanticTokens
-- used for debug and test
-- this function is used to recover the original tokens(with token in haskell token type zoon)
-- from the lsp semantic tokens(with token in lsp token type zoon)
recoverSemanticTokens :: String -> SemanticTokens -> Either Text [SemanticTokenOriginal]
recoverSemanticTokens sourceCode (SemanticTokens _ xs) = fmap (fmap $ tokenOrigin sourceCode) $ dataActualToken xs
    where
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
