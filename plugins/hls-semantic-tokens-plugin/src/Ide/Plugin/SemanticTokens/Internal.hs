{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Ide.Plugin.SemanticTokens.Internal where

import           Control.Lens                    ((^.))
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe       (MaybeT (..))
import           Data.Data                       (Data)
import           Data.Generics                   (everything, mkQ)
import           Data.List                       (sortBy)
import qualified Data.List                       as List
import           Data.Maybe                      (listToMaybe, mapMaybe)
import           Development.IDE                 (Action, GetHieAst (GetHieAst),
                                                  HieAstResult (HAR, hieAst),
                                                  IdeState,
                                                  TypeCheck (TypeCheck),
                                                  realSpan)
import           Development.IDE.Core.Compile    (TcModuleResult (..))
import           Development.IDE.Core.Rules      (getSourceFileSource,
                                                  runAction)
import           Development.IDE.Core.Shake      (use)
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Error                (getNormalizedFilePathE)
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
-- import Language.LSP.Protocol.Types.Common
import qualified Data.Text                       as T
import           Ide.Plugin.SemanticTokens.Query
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
-- import System.FilePath (takeExtension)
import           Control.Arrow                   ((&&&), (+++))
import           Control.Monad.Trans.Class       (lift)
import           Data.ByteString                 (ByteString, unpack)
import           Data.Generics                   (Typeable)
import           Data.List.Extra                 (chunksOf)
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Data.Typeable                   (cast)
import           Development.IDE                 (IdeState, Priority (..),
                                                  ideLogger, logPriority)

logWith :: (Show a, MonadIO m) => IdeState -> a -> m ()
logWith st = liftIO . logPriority (ideLogger st) Info . T.pack . show

bytestringString :: ByteString -> String
bytestringString = map (toEnum . fromEnum) . unpack

computeSemanticTokens' ::  forall a . String -> HieAST a -> Action (Maybe ())
computeSemanticTokens' src hieAst = do
    -- let identifiers = map NIdentifier $ identifierGetter hieAst
    let identifiersGroups = (map .map) NIdentifier $ toNameGroups $ identifierGetter hieAst

    liftIO $ mapM_ (\gr ->  liftIO (putStrLn $ "group size: " <> show (List.length gr)) >> mapM_ (\x -> putStrLn $ getOriginalTextFromId src x <> ":" <> show x) gr) identifiersGroups
    -- liftIO $ print $ "identifiers size: " <> show ( identifiers)
    pure $ Just ()

-----------------------
---- the api
-----------------------
computeSemanticTokens :: NormalizedFilePath -> Action (Maybe SemanticTokens)
computeSemanticTokens nfp = runMaybeT $ do
    HAR{hieAst} <- MaybeT $ use GetHieAst nfp
    source :: ByteString <- lift $ getSourceFileSource nfp
    let xs = Map.toList $ getAsts hieAst
    case xs of
        (x:_) -> do
            -- liftIO $ putStrLn $ "computeSemanticTokens': " <> show (fst x)
            -- MaybeT $ computeSemanticTokens' (bytestringString source) $ snd x
            tcM <- MaybeT $ use TypeCheck nfp
            case extractSemanticTokens' (snd x) $ tmrRenamed tcM of
                Right tokens -> pure tokens
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
            content <- liftIO $ readFile $ fromNormalizedFilePath nfp
            liftIO $ mapM_ print $ recoverSemanticTokens content items
            pure $ InL items


-----------------------
---- recover tokens
-----------------------

recoverSemanticTokens :: String -> SemanticTokens -> [SemanticTokenOriginal]
recoverSemanticTokens sourceCode (SemanticTokens _ xs) = map (tokenOrigin sourceCode) $ recoverSemanticToken xs

tokenOrigin sourceCode (line, startChar, len, tokenType, _) = SemanticTokenOriginal tokenType (Loc line startChar len) name
        where tLine = lines sourceCode !! (fromIntegral line-1)
              name = take (fromIntegral len) $ drop (fromIntegral startChar-0) tLine

-- every five elements is a token
-- recoverSemanticToken :: [UInt] -> [SemanticTokenData]
recoverSemanticToken xs =
    recoverPosition $
    if length xs `mod` 5 /= 0
    then panic "recoverSemanticToken: impossible"
    else map toTuple $ chunksOf 5 $ map fromIntegral xs
    where
        --   toTuple :: [UInt] -> SemanticTokenData
          toTuple [a, b, c, d, e] = (a, b, c, fromLspTokenType $ intToType d, e)
          toTuple _               = panic "recoverSemanticToken: impossible"
          -- recover to absolute position
        --   recoverPosition :: [SemanticTokenData] -> [SemanticTokenData]
          recoverPosition xs = ls $ foldl f (1, 0, []) xs
              where
                  f (lastLine, lastStartChar, acc) (line, startChar, len, tokenType, tokenModifiers)
                      = let newStartChar = if line == 0 then startChar + lastStartChar else startChar
                            newline = line + lastLine
                          in
                          (newline,newStartChar,
                          (newline,newStartChar, len, tokenType, tokenModifiers) :acc)
                  ls (_, _, acc) = List.reverse acc


