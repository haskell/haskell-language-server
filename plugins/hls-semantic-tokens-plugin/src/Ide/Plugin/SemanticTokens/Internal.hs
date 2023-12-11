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
{-# LANGUAGE TypeApplications      #-}

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
                                                       GetBindings (GetBindings),
                                                       GetHieAst (GetHieAst),
                                                       HieAstResult (HAR, hieAst, hieModule, refMap),
                                                       IdeState,
                                                       TypeCheck (TypeCheck),
                                                       realSpan,
                                                       useWithStaleFast)
import           Development.IDE.Core.Compile         (TcModuleResult (..))
import           Development.IDE.Core.Rules           (getSourceFileSource,
                                                       runAction)
import           Development.IDE.Core.Shake           (ShakeExtras (ShakeExtras),
                                                       getShakeExtras, use,
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
import           Data.Function                        (on)
import           Data.Generics                        (Typeable)
import           Data.List.Extra                      (chunksOf, (!?))
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import           Data.Typeable                        (cast)
import           Development.IDE                      (IdeState, Priority (..),
                                                       ideLogger, logPriority)
import           Development.IDE.Core.PositionMapping (zeroMapping)
import           Development.IDE.Spans.LocalBindings  (getDefiningBindings,
                                                       getLocalScope)

logWith :: (Show a, MonadIO m) => IdeState -> a -> m ()
logWith st = liftIO . logPriority (ideLogger st) Info . T.pack . show

bytestringString :: ByteString -> String
bytestringString = map (toEnum . fromEnum) . unpack

-- debugComputeSemanticTokens ::  forall a . String -> HieAST a -> RenamedSource -> Action (Maybe ())
-- debugComputeSemanticTokens src hieAst rs = do
--     let identifiersGroups = (map . map) NIdentifier
--             $ toNameGroups
--             -- $ filter (\(_, name, _) -> name `List.elem` ns)
--             $ identifierGetter hieAst
--     liftIO $ mapM_ (\gr ->  liftIO (putStrLn $ "group size: " <> show (List.length gr)) >> mapM_ (\x -> putStrLn $
--         getOriginalTextFromId src x
--         <> ":"
--         <> show x) gr)
--         identifiersGroups
--     -- liftIO $ print $ "identifiers size: " <> show ( identifiers)
--     pure $ Just ()
--     where
--         toNameGroups :: [IdentifierItem] -> [[IdentifierItem]]
--         toNameGroups = List.groupBy ((==) `on` identifierItemName) . List.sortOn identifierItemName
--         identifierItemName :: IdentifierItem -> Name
--         identifierItemName (span, x, y) = x

-----------------------
---- the api
-----------------------

computeSemanticTokens :: NormalizedFilePath -> Action (Maybe SemanticTokens)
computeSemanticTokens nfp =
        runMaybeT $ do
    -- HAR{hieAst, refMap} <- MaybeT $ use GetHieAst nfp
    HAR{..} <- MaybeT $ use GetHieAst nfp
    liftIO $ putStrLn $ "moduleName: " <> showSDocUnsafe (ppr hieModule)
    -- (ImportMap imports, _) <- MaybeT $ useWithStaleFastMT GetImportMap file
    source :: ByteString <- lift $ getSourceFileSource nfp
    let xs = Map.toList $ getAsts hieAst
    liftIO $ print $ "hieAst size: " <> show (List.length xs)

    case xs of
        ((_,ast):_) -> do
            binds <- MaybeT $ use GetBindings nfp
            let importedNames = importedNameFromModule hieModule ast
            ShakeExtras{withHieDb} <- MaybeT $ fmap Just getShakeExtras
            nameRefPaths <- getNamesRefs withHieDb importedNames
            importedModuleNameSemanticMap <- MaybeT $ fmap Just $ computeImportedSemanticTokens nameRefPaths
            let originalModuleNameSemanticMap = toNameSemanticMap refMap
            let combineMap = Map.unionWith (<>) originalModuleNameSemanticMap importedModuleNameSemanticMap
            let names = identifierGetter ast
            let moduleAbsTks = extractSemanticTokensFromNames combineMap names
            case semanticTokenAbsoluteSemanticTokens moduleAbsTks of
                Right tokens -> do
                    liftIO $ mapM_ (\x -> mapM_ print x) $ recoverSemanticTokens (bytestringString source) tokens
                    pure tokens
                Left err -> do
                    liftIO $ putStrLn $ "computeSemanticTokens: " <> show err
                    MaybeT . pure $ Nothing
        _ -> MaybeT . pure  $ Nothing


semanticTokensFull :: PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull state _ param = do
    let dbg = logWith state
    nfp <-  getNormalizedFilePathE (param ^. (L.textDocument . L.uri))
    dbg $ "semanticTokensFull: " <> show nfp
    items <- liftIO
        $ runAction "SemanticTokens.semanticTokensFull" state
        $ computeSemanticTokens nfp
    case items of
        Nothing -> pure $ InR Null
        Just items -> do
            -- content <- liftIO $ readFile $ fromNormalizedFilePath nfp
            -- liftIO $ mapM_ (mapM_ print) $ recoverSemanticTokens content items
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
computeImportedSemanticTokens :: [NormalizedFilePath] -> Action NameSemanticMap
computeImportedSemanticTokens namPathMap = do
    x <- forM namPathMap $ \nfp -> runMaybeT $ do
            HAR{hieAst, refMap} <- MaybeT $ use GetHieAst nfp
            binds <- MaybeT $ use GetBindings nfp
            return $ toNameSemanticMap refMap
    let xs = catMaybes x
    return $ Map.unionsWith (<>) xs
