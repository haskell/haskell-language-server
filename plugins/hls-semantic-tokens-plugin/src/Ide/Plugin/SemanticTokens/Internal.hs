{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
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
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
-- import System.FilePath (takeExtension)
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


deriving instance Show DeclType
deriving instance Show BindType
deriving instance Show RecFieldContext
-----------------------
---- the api
-----------------------
instance Show ContextInfo where
    show x = case x of
        Use                   -> "Use"
        MatchBind             -> "MatchBind"
        IEThing _             -> "IEThing IEType" -- imported
        TyDecl                -> "TyDecl"
        ValBind bt _ _        -> "ValBind of " <> show bt
        PatternBind _ _ _     -> "PatternBind"
        ClassTyDecl _         -> "ClassTyDecl"
        Decl d _              -> "Decl of " <> show d
        TyVarBind _ _         -> "TyVarBind"
        RecField c _          -> "RecField of " <> show c
        EvidenceVarBind _ _ _ -> "EvidenceVarBind"
        EvidenceVarUse        -> "EvidenceVarUse"

instance Show (IdentifierDetails a) where
    show x = show $ identInfo x


getOriginalText :: String -> Span -> String
getOriginalText sourceCode span = take len $ drop (startChar-1) tLine
        where tLine = lines sourceCode !! (line-1)
              line = srcSpanStartLine span
              startChar = srcSpanStartCol span
              len= srcSpanEndCol span - startChar

bytestringString :: ByteString -> String
bytestringString = map (toEnum . fromEnum) . unpack
-- para :: MessageParams 'Method_TextDocumentSemanticTokensFull
-- para = _
computeSemanticTokens :: NormalizedFilePath -> Action (Maybe SemanticTokens)
computeSemanticTokens nfp = runMaybeT $ do
    -- HAR{hieAst} <- MaybeT $ use GetHieAst nfp
    -- source :: ByteString <- lift $ getSourceFileSource nfp

    -- let xs = Map.toList $ getAsts hieAst
    -- liftIO $ putStrLn $ "size" <> show (List.length xs)
    -- -- typedAst <- MaybeT $ pure $ cast hieAst
    -- case xs of
    --     (x:_) -> do
    --         liftIO $ putStrLn $ "computeSemanticTokens': " <> show (fst x)
    --         MaybeT $ computeSemanticTokens' (bytestringString source) $ snd x
    --     _ -> pure ()

    tcM <- MaybeT $ use TypeCheck nfp
    pure $ sourceToTokens $ tmrRenamed tcM

computeSemanticTokens' ::  forall a . String -> HieAST a -> Action (Maybe ())
computeSemanticTokens' src hieAst = do
    let identifiers = map NIdentifier $ identifierGetter hieAst
    liftIO $ mapM_ (\x -> putStrLn $ getOriginalTextFromId src x <> ":" <> show x) identifiers
    -- liftIO $ print $ "identifiers size: " <> show ( identifiers)
    pure $ Just ()



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
            -- dbg $ unlines $ map show $ recoverSemanticTokens content items
            pure $ InL items

-----------------------
---- convert to lsp
-----------------------

recoverSemanticTokens :: String -> SemanticTokens -> [SemanticTokenOriginal]
recoverSemanticTokens sourceCode (SemanticTokens _ xs) = map (tokenOrigin sourceCode) $ recoverSemanticToken xs

tokenOrigin :: String -> SemanticTokenData -> SemanticTokenOriginal
tokenOrigin sourceCode (line, startChar, len, tokenType, _) = SemanticTokenOriginal (toEnum tokenType) (Loc line startChar len) name
        where tLine = lines sourceCode !! (line-1)
              name = take len $ drop (startChar-0) tLine


-- every five elements is a token
recoverSemanticToken :: [UInt] -> [SemanticTokenData]
recoverSemanticToken xs =
    recoverPosition $
    if length xs `mod` 5 /= 0
    then panic "recoverSemanticToken: impossible"
    else map toTuple $ chunksOf 5 $ map fromIntegral $ xs
    where toTuple [a, b, c, d, e] = (a, b, c, d, e)
          toTuple _               = panic "recoverSemanticToken: impossible"
          -- recover to absolute position
          recoverPosition :: [SemanticTokenData] -> [SemanticTokenData]
          recoverPosition xs = ls $ foldl f (1, 0, []) xs
              where
                  f (lastLine, lastStartChar, acc) (line, startChar, len, tokenType, tokenModifiers)
                      = let newStartChar = if line == 0 then startChar + lastStartChar else startChar
                            newline = line + lastLine
                          in
                          (newline,newStartChar,
                          (newline,newStartChar, len, tokenType, tokenModifiers) :acc)
                  ls (_, _, acc) = List.reverse acc



sourceToTokens :: RenamedSource -> SemanticTokens
sourceToTokens = toLspSemanticTokens . toSemanticTokens . nameGetter

emptySemanticTokens :: SemanticTokens
emptySemanticTokens = SemanticTokens Nothing []

toLspSemanticTokens :: [SemanticToken] -> SemanticTokens
toLspSemanticTokens xs = SemanticTokens Nothing (concatMap toTokenInt xs)


-- semanticTokenToString :: SemanticToken -> String
-- semanticTokenToString ((line, startChar, len, tokenType, tokenModifiers), locName) =
--     show line ++ ":" ++ show startChar ++ ":" ++ show len ++ ":" ++ show tokenType ++ ":" ++ show tokenModifiers ++ ":\n" ++ collectToString locName

toTokenInt :: SemanticToken -> [UInt]
toTokenInt ((line, startChar, len, tokenType, tokenModifiers), _) =
    [fromIntegral line, fromIntegral startChar, fromIntegral len, fromIntegral tokenType, fromIntegral tokenModifiers]

-- need to take offset
toSemanticTokens :: [SemanticCollectFinal] -> [SemanticToken]
toSemanticTokens = computeOffsets . List.sortOn fst. mapMaybe toSemanticToken
    where
        computeOffsets :: [SemanticToken] -> [SemanticToken]
        computeOffsets = ls . foldl f (1, 0, [])
        f (lastLine, lastStartChar, acc) ((line, startChar, len, tokenType, tokenModifiers), locName)
            = ( line, startChar,
                (if lastLine == line then
                    (0, startChar - lastStartChar, len, tokenType, tokenModifiers)
                else
                    (line - lastLine, startChar, len, tokenType, tokenModifiers)
                    , locName) :acc)
        ls (_, _, acc) = List.reverse acc

toSemanticToken :: SemanticCollectFinal -> Maybe SemanticToken
toSemanticToken ori@(tokenType, locName) = do
    loc <- realSpan $ getLocA locName
    let line = srcSpanStartLine loc
    let startChar = srcSpanStartCol loc
    let len= srcSpanEndCol loc - startChar
    return
        -- vscode render col start from 0
        ((line, startChar-1, len, fromEnum tokenType, 0), ori)


-----------------------
---- collect from ast
-----------------------


toTokenType :: LIdP GhcRn -> SemanticTokenType
toTokenType locName = case occNameSpace $ occName $ unLoc locName of
  x | isTcClsNameSpace x   -> SClass
  x | isTvNameSpace x      -> STypeVar
  x | isDataConNameSpace x -> SDataCon
  x | isVarNameSpace x     -> SVariable
  _                        -> SNothing

nameToCollect :: LIdP GhcRn -> [SemanticCollectFinal]
nameToCollect locName = [(toTokenType locName, locName)]

nameGetter :: RenamedSource -> [SemanticCollectFinal]
nameGetter = everything (++) ([] `mkQ` nameToCollect)



-- identifier from source
-- isSrcIdentifier :: Identifier -> Bool

-- filter generated identifiers

-- from declaration site to token type
type TokenTypeMap  = Map.Map Span SemanticTokenTypeI
type TokenTypeItem = (Span, SemanticTokenTypeI)
type IdentifierItem = (Span, Maybe SrcSpan, Identifier, [ContextInfo])


newtype NIdentifier = NIdentifier IdentifierItem

instance Show NIdentifier where
    show (NIdentifier (span, c,Left x, y)) =  "module:" <> show x <> " " <> show y <> " " <>  printCompactRealSrc span
    show (NIdentifier (span, c, Right x, y)) = show (nameOccName x) <> " " <> show y <> ""
            <> maybe "" printCompactSrcSpan c <> " " <> printCompactRealSrc span

addToTokenTypeMap :: IdentifierItem -> Maybe TokenTypeItem
addToTokenTypeMap (_, Nothing, _, _) = Nothing
addToTokenTypeMap (span, _ , _, contexts)
    | Just ctx <- declInfo contexts
        = case ctx of
            Decl ClassDec _     -> Just (span, TClass)
            Decl DataDec  _     -> Just (span, TDataCon)
            Decl ConDec   _     -> Just (span, TTypeCon)
            Decl SynDec   _     -> Just (span, TTypeSyn)
            Decl FamDec   _     -> Just (span, TTypeFam)
            -- nothing for instance dec
            Decl InstDec  span  -> Nothing
            -- todo pattern syn
            Decl PatSynDec span -> Nothing
    -- | Just ctx <- valBindInfo contexts
    --     = case ctx of
    --         ValBind FunBind _ _ -> Just (span, TFunction)
    --         ValBind PatBind _ _ -> Just (span, TVariable)
    --         ValBind VarBind _ _ -> Just (span, TVariable)
    --         ValBind PatSynBind _ _ -> Nothing
    | otherwise = Nothing


recFieldInfo, declInfo, valBindInfo, classTyDeclInfo,
    useInfo, patternBindInfo, tyDeclInfo, matchBindInfo :: [ContextInfo] -> Maybe ContextInfo
recFieldInfo    ctxs = listToMaybe [ctx       | ctx@RecField{}    <- ctxs]
declInfo        ctxs = listToMaybe [ctx       | ctx@Decl{}        <- ctxs]
valBindInfo     ctxs = listToMaybe [ctx       | ctx@ValBind{}     <- ctxs]
classTyDeclInfo ctxs = listToMaybe [ctx       | ctx@ClassTyDecl{} <- ctxs]
useInfo         ctxs = listToMaybe [Use       | Use               <- ctxs]
patternBindInfo ctxs = listToMaybe [ctx       | ctx@PatternBind{} <- ctxs]
tyDeclInfo      ctxs = listToMaybe [TyDecl    | TyDecl            <- ctxs]
matchBindInfo   ctxs = listToMaybe [MatchBind | MatchBind         <- ctxs]




-- | Recurses through the given AST to find identifiers which are
-- 'InstanceValBind's.
identifierGetter :: HieAST a -> [IdentifierItem]
identifierGetter ast =
    let ids = [ (nodeSpan ast, getIdSrcSpan c, c, Set.toList $ identInfo d) | (c, d) <- Map.toList $ getNodeIds ast]
    in ids <> concatMap identifierGetter (nodeChildren ast)
    where getIdSrcSpan :: Identifier -> Maybe SrcSpan
          getIdSrcSpan (Left _)  = Nothing
          getIdSrcSpan (Right x) = Just $ nameSrcSpan x

printCompactSrcSpan :: SrcSpan -> String
printCompactSrcSpan (RealSrcSpan x _buf) = printCompactRealSrc x
printCompactSrcSpan x                    = "noSrc"

printCompactRealSrc :: RealSrcSpan -> String
printCompactRealSrc x = showSDocUnsafe $ ppr x



getOriginalTextFromId :: String -> NIdentifier -> String
getOriginalTextFromId sourceCode (NIdentifier (span, c, _, _)) = getOriginalText sourceCode span

-- identifierGetter :: Data a => HieAST a -> [(Identifier, IdentifierDetails a)]
-- identifierGetter = everything (++) ([] `mkQ` identifierToCollect)

-- identifierToCollect :: Data a => NodeIdentifiers a -> [(Identifier, IdentifierDetails a)]
-- identifierToCollect xs = Map.toList xs
