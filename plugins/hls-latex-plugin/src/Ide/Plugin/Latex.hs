{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}

module Ide.Plugin.Latex where

import           Control.Applicative                                 (liftA2,
                                                                      liftA3)
import           Control.Exception                                   (SomeException (SomeException),
                                                                      catch,
                                                                      try)
import           Control.Monad                                       (foldM_,
                                                                      (>=>))
import           Control.Monad.IO.Class                              (liftIO)
import           Data.Aeson                                          (FromJSON,
                                                                      ToJSON,
                                                                      Value (..),
                                                                      toJSON)
import           Data.Foldable                                       (toList)
import           Data.List                                           (intercalate,
                                                                      intersperse)
import           Data.Maybe                                          (catMaybes,
                                                                      mapMaybe)
import qualified Data.Text                                           as Text
import qualified Data.Text.IO                                        as Text
import qualified Debug.Trace                                         as Debug
import           Development.IDE                                     hiding
                                                                     (PFailed,
                                                                      POk,
                                                                      ParseResult,
                                                                      align,
                                                                      braces,
                                                                      indent,
                                                                      runParser,
                                                                      space)
import           Development.IDE.Core.Compile                        (lookupName)
import qualified Development.IDE.GHC.Compat                          as GHC
import qualified Development.IDE.GHC.Compat.Util                     as GHC
import           Development.IDE.Plugin.TypeLenses                   (GetGlobalBindingTypeSigs (..),
                                                                      GlobalBindingTypeSig (..),
                                                                      GlobalBindingTypeSigsResult (..))
import           Development.IDE.Spans.Common                        (safeTyThingType)
import           Development.IDE.Spans.LocalBindings                 (getDefiningBindings)
import           Generics.SYB                                        (listify,
                                                                      mkQ,
                                                                      something)
import           "ghc-lib-parser" GHC.Data.EnumSet                   (empty)
import           "ghc-lib-parser" GHC.Data.FastString                (FastString,
                                                                      mkFastString)
import           "ghc-lib-parser" GHC.Data.StringBuffer              (stringToStringBuffer)
import           "ghc-lib-parser" GHC.Driver.Session                 (DynFlags,
                                                                      defaultDynFlags)
import           "ghc-lib-parser" GHC.Hs
import           GHC.IO                                              (unsafePerformIO)
import           "ghc-lib-parser" GHC.Parser                         (parseModule)
import           "ghc-lib-parser" GHC.Parser.Lexer                   (P (unP),
                                                                      ParseResult (..),
                                                                      ParserOpts,
                                                                      initParserState,
                                                                      mkParserOpts)
import           "ghc-lib-parser" GHC.Types.Name
import           "ghc-lib-parser" GHC.Types.Name.Reader
import           "ghc-lib-parser" GHC.Types.SrcLoc                   (Located,
                                                                      SrcLoc,
                                                                      mkGeneralSrcLoc,
                                                                      mkRealSrcLoc,
                                                                      mkSrcLoc,
                                                                      mkSrcSpan,
                                                                      unLoc)
import qualified "ghc" GHC.Types.Unique                              as GHC
import           "ghc-lib-parser" GHC.Types.Unique                   (Unique,
                                                                      mkUnique)
import           "ghc-lib-parser" GHC.Utils.Outputable               (Outputable,
                                                                      ppr,
                                                                      showSDocUnsafe)
import           Ide.Plugin.StringToLatex                            (MapOpSymbol,
                                                                      fToLatex,
                                                                      mapSymbol,
                                                                      opToLatex)
import           Ide.Types
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeLlvmConfig,
                                                                      fakeSettings)
import           Language.Haskell.TH                                 (mkName)
import           Language.LSP.Server                                 (sendNotification)
import           Language.LSP.Types
import qualified Language.LSP.Types                                  as LSP
import           System.Directory                                    (doesFileExist)
import           System.FilePath                                     (replaceExtension)
import           System.IO                                           (BufferMode (NoBuffering),
                                                                      hPutStrLn,
                                                                      hSetBuffering,
                                                                      hShow)
import           System.IO.Temp                                      (withSystemTempFile,
                                                                      withTempFile)
import           System.Process                                      (callCommand)
import           Text.LaTeX.Base.Class                               (braces,
                                                                      commS,
                                                                      squareBraces)
import           Text.LaTeX.Base.Commands                            (array,
                                                                      between,
                                                                      document,
                                                                      documentclass,
                                                                      huge,
                                                                      indent,
                                                                      leftarrow,
                                                                      lnbk, raw,
                                                                      rightarrow,
                                                                      usepackage,
                                                                      (&))
import           Text.LaTeX.Base.Render                              (render)
import           Text.LaTeX.Base.Syntax                              (LaTeX (TeXEnv),
                                                                      MathType,
                                                                      Measure,
                                                                      TeXArg)
import           Text.LaTeX.Base.Types                               (TableSpec (..))
import           Text.LaTeX.Packages.AMSMath                         (align,
                                                                      amsmath,
                                                                      autoBraces,
                                                                      autoParens,
                                                                      autoSquareBrackets,
                                                                      cases,
                                                                      equation_,
                                                                      mathrm,
                                                                      space)
import           Text.LaTeX.Packages.AMSSymb                         (amssymb)
import           Text.Printf                                         (printf)

data Log = LogLatex String
  deriving Show

instance Pretty Log where
  pretty = \case
    LogLatex log -> viaShow log


data HaskellToLatexError = NotSupportedError String
                         | ImageTooLargeError String
                         | FileParseError String
                         | NoBindingsFound String
                         deriving Show

showError :: HaskellToLatexError -> String
showError (NotSupportedError s) = "Function contains non-supported syntax: " ++ s
showError (ImageTooLargeError s) = "Function generates too large figure: " ++ s
showError (FileParseError s) = "Could not parse file: " ++ s
showError (NoBindingsFound s) = "Could not find any bindings at cursor: " ++ s

deriving instance FromJSON LaTeX
deriving instance FromJSON Measure
deriving instance FromJSON TeXArg
deriving instance FromJSON MathType

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
                    <> mkPluginHandler STextDocumentHover renderLatexOnHover
    , pluginCommands = [PluginCommand "sendLatexNotification"
                                      "Show Latex for this function"
                                      sendLatexNotificationCommand
                       ]
    }

renderLatexOnHover :: PluginMethodHandler IdeState TextDocumentHover
renderLatexOnHover ideState _ (HoverParams (TextDocumentIdentifier uri) position _)
  | Just docNormalizedFilePath <- uriToNormalizedFilePath (toNormalizedUri uri)
  = liftIO $ do
    eBindings <- getBindingsAtCursor ideState (Range position position) docNormalizedFilePath
    case eBindings of
      Right bindings -> do
        eLatex <- bindingsToLatex ideState bindings docNormalizedFilePath
        case eLatex of
          Right latex -> do
            withTempFile "/tmp/" "template.tex" $ \fp hdl -> do
              hSetBuffering hdl NoBuffering
              Text.hPutStrLn hdl (render (templateFile latex))

              er <- try @SomeException (script fp)
              case er of
                Left e   -> return $ hoverContents (show e)
                Right () -> do
                  let png = replaceExtension fp ".png"
                  return $ hoverContents ("![](" ++ png ++ ")")

          Left e -> return $ hoverContents (showError e)
      Left e -> return $ hoverContents (showError e)
  | otherwise =
    return $ hoverContents ("Latex Plugin error: Could not normalize uri: " ++ show uri)
  where
    hoverContents string =
      Right $ Just
            $ Hover
                (HoverContents
                  (unmarkedUpContent
                    (Text.pack string)))
                Nothing
    -- https://superuser.com/questions/1582448/convert-mathematical-formula-to-image-locally-on-linux
    templateFile :: [LaTeX] -> LaTeX
    templateFile latex = documentclass [] "standalone"
                      <> usepackage [] amsmath
                      <> usepackage [] amssymb
                      <> (document
                           $ huge
                             (equation_
                           $ TeXEnv "aligned" []
                           $ mconcat
                           $ intersperse lnbk latex)
                         )

    script file = callCommand
                $ intercalate "\n"
                  [ clearPNGFiles file
                  , generatePDF file
                  , pdfToPNG file
                  , clearTexFiles file
                  ]

    generatePDF file = "pdflatex -interaction=nonstopmode -output-directory /tmp/ "
                    ++ file
    pdfToPNG file = "convert -trim -flatten "
                 ++ replaceExtension file ".pdf"
                 ++ " "
                 ++ replaceExtension file ".png"
    clearTexFiles file = "rm /tmp/*.tex /tmp/*.aux /tmp/*.log /tmp/*.pdf"
    clearPNGFiles file = "rm /tmp/*.png"

sendLatexNotificationCommand :: CommandFunction IdeState Text.Text
sendLatexNotificationCommand ide latex = do
  sendNotification SWindowShowMessage (ShowMessageParams MtInfo latex)
  pure (Right Null)

codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState pId (CodeActionParams _ _ (TextDocumentIdentifier uri) range _)
  | Just docNormalizedFilePath <- uriToNormalizedFilePath (toNormalizedUri uri)
  = liftIO $ fmap (Right . LSP.List . map LSP.InR) $ do
    mbBindings <- getBindingsAtCursor ideState range docNormalizedFilePath
    case mbBindings of
      Right bindings -> do
        eLatex <- bindingsToLatex ideState bindings docNormalizedFilePath
        case eLatex of
          Right latex -> do
            return
              [mkCodeAction "Show Latex for this function"
                            "sendLatexNotification"
                            "Show Latex for this function"
                            (render (mconcat latex))
              ]

          Left e ->
            return
              [mkCodeAction "Show Latex for this function"
                            "sendLatexNotification"
                            "Show Latex for this function"
                            (showError e)
              ]
      Left _ -> return []
  | otherwise
  = do
    return $ Right $ LSP.List []
  where
    mkCodeAction title cmdId cmdTitle latex =
      CodeAction { _title = Text.pack title
                 , _kind = Nothing
                 , _diagnostics = Nothing
                 , _isPreferred = Nothing
                 , _disabled = Nothing
                 , _edit = Nothing
                 , _command = Just
                            $ mkLspCommand pId
                                           (CommandId (Text.pack cmdId))
                                           (Text.pack cmdTitle)
                                           (Just [toJSON latex])
                 , _xdata = Nothing
                 }

getBindingsAtCursor :: IdeState -> Range -> NormalizedFilePath -> IO (Either HaskellToLatexError [Name])
getBindingsAtCursor ideState range uri = do
  -- Get all bindings function bindings in order to only get code actions for these
  mbBindings <- runAction "latex.GetBindings" ideState (use GetBindings uri)
  case mbBindings of
       Just bindings -> do
         let userRealSrcSpan             = rangeToRealSrcSpan uri range
             definedBindingNamesInCursor =
               map (nameToName . fst) (getDefiningBindings bindings userRealSrcSpan)
         if null definedBindingNamesInCursor
            then return (Left (NoBindingsFound ("No binding at cursor found: " ++ show range)))
            else return (Right definedBindingNamesInCursor)
       Nothing -> return (Left (NoBindingsFound ("No binding at cursor found: " ++ show range)))

bindingsToLatex :: IdeState -> [Name] -> NormalizedFilePath -> IO (Either HaskellToLatexError [LaTeX])
bindingsToLatex ideState bindings uri = do
  mbContents <- runAction "latex.GetFileContents" ideState (use GetFileContents uri)

  case mbContents of
    Just (_, Just contents) -> do
      let testest = runParser (fromNormalizedFilePath uri) (Text.unpack contents) parseModule

      case testest of
        POk _ parsedSource -> do
          let definedBindingOccNamesInCursor = map nameOccName bindings
              functionBindings               = concatMap (findFunctionBinding parsedSource)
                                                         definedBindingOccNamesInCursor
              latexFunctions                 = functionBindingToLatex <$> functionBindings
          return (sequence latexFunctions)
        PFailed {} -> do
          return $ Left $ FileParseError ("Could not parse file contents: " ++ show uri)
    _ -> return $ Left $ FileParseError ("Could not get file contents: " ++ show uri)

  where
    -- Looks for all 'HsBindLR' types that have the 'FunBind' constructor and
    -- extract the payload ('MatchGroup idR (LHsExpr idR)'). 'FunBind'
    -- constructor corresponds to a function definition. After finding one we
    -- check if this function's id is the same as the one we are looking for.
    --
    -- NOTE: Since 'LIdP' type has a 'RdrName' I tried to convert all binding
    -- names to 'RdrName' type and compare them in order to find the 'HsBindLR'
    -- (aka function definition in the AST) that corresponds to the function I
    -- am looking for. However the 'RdrName' has a weird equality instance and
    -- after some trial and error converting 'RdrName' to 'OccName' worked.
    findFunctionBinding :: Located HsModule -> OccName -> [MatchGroup GhcPs (LHsExpr GhcPs)]
    findFunctionBinding parsedSource name =
      map fun_matches
      . listify @(HsBindLR GhcPs GhcPs)
          (\case
            FunBind _ locId _ _ ->
              rdrNameOcc (unLoc locId) == name
            _ -> False)
      . map unLoc
      . hsmodDecls
      . unLoc
      $ parsedSource

    -- MatchGroup has a list of matches, each item in this list is a different
    -- equation for the same match, e.g.:
    -- myFunction 0 = 0
    -- myFunction 1 = 1
    -- myFunction a = a - myFunction (a - 1)
    --
    -- This function's list of matches will have 3 elements.
    --
    -- To transform this construcotr into Latex we check if the function has
    -- more than 1 equation, if it does then we add aligning primitives to the
    -- beginning of each equation.
    functionBindingToLatex :: MatchGroup GhcPs (LHsExpr GhcPs) -> Either HaskellToLatexError LaTeX
    functionBindingToLatex matchGroup =
      let matches = map unLoc (unLoc (mg_alts matchGroup))
       in case matches of
        []      -> Right mempty
        [match] -> lMatchToLatex match
        _       -> fmap (mconcat . intersperse lnbk) (mapM (fmap (mempty &) . lMatchToLatex) matches)

    -- A Match can be various things, the most common being a function.
    -- If the match is a function then we have the list of different pattern
    -- matches and their respective bodies. To transform into Latex it's easy
    -- enough.
    --
    -- If the match is a lambda expression it is also easy enough, just print a
    -- pretty latex lambda
    --
    -- If the match is a case expression be a little more careful with the latex
    -- equivalent but that's delegated to a differente function
    --
    -- For now we only support these types of match contexts.
    lMatchToLatex :: Match GhcPs (LHsExpr GhcPs) -> Either HaskellToLatexError LaTeX
    lMatchToLatex m@Match { m_ctxt, m_pats, m_grhss } =
      case m_ctxt of
        FunRhs lId _ _ -> fmap (\x -> mathrm (outputableToString lId) <> space
                                   <> mconcat (intersperse space (map (patToLatex . unLoc) m_pats))
                                   <> raw "=" <> x
                               )
                               (grhssToLatex m_grhss)
        LambdaExpr -> fmap (\x -> autoParens
                                $ between (commS "backslash") space space
                               <> mconcat (intersperse space (map (patToLatex . unLoc) m_pats))
                               <> space <> rightarrow <> space
                               <> x
                           )
                           (grhssToLatex m_grhss)
        CaseAlt -> fmap (\x -> mconcat (intersperse space (map (patToLatex . unLoc) m_pats))
                            <> between rightarrow space space
                            <> x
                        )
                        (grhssToLatex m_grhss)
        _ -> Left (NotSupportedError "This function uses match contexts which are not supported yet")

    -- This function pretty prints patterns to Latex.
    -- Just need to be careful regarding certain symbols.
    patToLatex :: Pat GhcPs -> LaTeX
    patToLatex (VarPat _ lIdP)      = outputableToString lIdP
    patToLatex (LazyPat _ lPat)     = patToLatex (unLoc lPat)
    patToLatex (AsPat _ lIdP lPat)  = between (raw "@") (outputableToString lIdP) (patToLatex (unLoc lPat))
    patToLatex (ParPat _ lPat)      = autoParens (patToLatex (unLoc lPat))
    patToLatex (BangPat _ lPat)     = raw "!" <> patToLatex (unLoc lPat)
    patToLatex (ListPat _ lPats)    = autoSquareBrackets (mconcat $ intersperse (mathrm "," <> space) (map (patToLatex . unLoc) lPats))
    patToLatex (TuplePat _ lPats _) = autoParens (mconcat $ intersperse (mathrm "," <> space) (map (patToLatex . unLoc) lPats))
    patToLatex (ConPat _ con args)  =
      case args of
        PrefixCon _ pats     -> mathrm (outputableToString con) <> space
                             <> (mconcat $ intersperse space (map (patToLatex . unLoc) pats))
        RecCon _           -> outputableToString args
        InfixCon pat1 pat2 -> patToLatex (unLoc pat1)
                           <> space <> raw "`" <> mathrm (outputableToString con) <> raw "`" <> space
                           <> patToLatex (unLoc pat2)
    patToLatex (SigPat _ pat ty)    = patToLatex (unLoc pat) <> between (raw "::") space space <> outputableToString ty
    patToLatex pat                  = outputableToString pat

    -- GRHSs are Guarded Right-Hand Sides, this means that if our guard list has
    -- more than 1 element, we are dealing with guards.
    --
    -- It's here that we also get locally binded values, e.g. where clause
    grhssToLatex :: GRHSs GhcPs (LHsExpr GhcPs) -> Either HaskellToLatexError LaTeX
    grhssToLatex GRHSs { grhssGRHSs, grhssLocalBinds } =
      case map unLoc grhssGRHSs of
        [] -> Right mempty
        [GRHS _ [] expr] -> liftA2 (<>) (hsExprToLatex (unLoc expr)) (hsLocalBindsToLatex grhssLocalBinds)
        l -> let lastGuard = last l
                 l'        = init l
              in liftA3 (\x y z -> cases (
                                   x
                                <> lnbk
                                <> y)
                                <> z
                      )
                      (fmap (mconcat . intersperse lnbk) (mapM (grhsToLatex False) l'))
                      (grhsToLatex True lastGuard)
                      (hsLocalBindsToLatex grhssLocalBinds)

    -- This deals with the local bindings. Right now we only support 'where' clauses
    hsLocalBindsToLatex :: HsLocalBinds GhcPs -> Either HaskellToLatexError LaTeX
    hsLocalBindsToLatex (HsValBinds _ (ValBinds _ bag _)) =
      let bagLatex = fmap (mconcat . intersperse (between (mathrm "and") space space))
                   . mapM (hsBindLRToLatex . unLoc)
                   . toList
                   $ bag
       in fmap (\x -> between (mathrm "where") space space <> x) bagLatex
    hsLocalBindsToLatex (EmptyLocalBinds _) = Right mempty
    hsLocalBindsToLatex _ = Left (NotSupportedError "This function uses a local bind that is not supported")

    -- This function deals with fucntion local bindings, it is slightly different
    -- than MatchGroups.
    --
    -- For now we only support the simplest forms
    hsBindLRToLatex :: HsBindLR GhcPs GhcPs -> Either HaskellToLatexError LaTeX
    hsBindLRToLatex (FunBind _ _ mg _) =
      let matches = map unLoc (unLoc (mg_alts mg))
       in fmap (mconcat . intersperse (between (mathrm "and") space space)) (mapM lMatchToLatex matches)
    hsBindLRToLatex _ = Left (NotSupportedError "This function uses a binding that is not yet supported")

    -- This function deaks with Guarded Right Hand Side clauses. The Bool argument's
    -- purpose is to distinguish the last guarded clause, usually left for the 'otherwise'
    -- keyword. Note that this is currently hard-coded and it might not be the case.
    grhsToLatex :: Bool -> GRHS GhcPs (LHsExpr GhcPs) -> Either HaskellToLatexError LaTeX
    grhsToLatex False (GRHS _ glstmt expr) = liftA2 (\x y -> x
                                                          <> mathrm "," <> space <> mathrm "if" <> space
                                                          <> y
                                                    )
                                                    (hsExprToLatex (unLoc expr))
                                                    (fmap (mconcat . intersperse (mathrm "," <> space)) . mapM (stmtLRToLatex . unLoc) $ glstmt)
    grhsToLatex True (GRHS _ _ expr) = fmap (\x -> x
                                                <> mathrm "," <> space <> mathrm "otherwise" <> space
                                            ) (hsExprToLatex (unLoc expr))

    stmtLRToLatex :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Either HaskellToLatexError LaTeX
    stmtLRToLatex (BodyStmt _ expr _ _) = hsExprToLatex (unLoc expr)
    stmtLRToLatex expr                  = Right
                                        $ outputableToString expr

    -- This function transforms the most common Haskell Expression constructors
    -- into Latex.
    --
    -- Currently does not support fancy constructors like do notation or let bindings
    --
    hsExprToLatex :: HsExpr GhcPs -> Either HaskellToLatexError LaTeX
    hsExprToLatex (HsVar _ lIdP)                      = Right $ outputableToString lIdP
    hsExprToLatex (HsLam _ matchGroup)                = functionBindingToLatex matchGroup
    hsExprToLatex (HsLamCase _ m)                     = fmap (\x -> cases
                                                                  $ rightarrow
                                                                 <> braces (x <> lnbk)
                                                             )
                                                             (functionBindingToLatex m)
    hsExprToLatex (HsApp _ exprF exprX)               = fToLatex <$> hsExprToLatex (unLoc exprF)
                                                                 <*> hsExprToLatex (unLoc exprX)
    hsExprToLatex (OpApp _ exprLeft exprOp exprRight) = opToLatex <$> hsExprToLatex (unLoc exprLeft)
                                                                  <*> hsExprToLatex (unLoc exprOp)
                                                                  <*> hsExprToLatex (unLoc exprRight)
    hsExprToLatex (NegApp _ expr _)                   = fmap (\x -> between (commS "neg") space space <> x)
                                                             (hsExprToLatex (unLoc expr))
    hsExprToLatex (HsPar _ expr)                      = autoParens <$> hsExprToLatex (unLoc expr)
    hsExprToLatex (HsCase _ expr matchGroup)          = liftA2 (\x y -> mathrm "case" <> space <> x <> between (mathrm "of") space space
                                                                     <> cases y
                                                               )
                                                               (hsExprToLatex (unLoc expr))
                                                               (functionBindingToLatex matchGroup)
    hsExprToLatex (HsIf _ exprCond exprThen exprElse) = liftA3 (\x y z-> cases $ x
                                                                 <> mathrm "," <> space <> mathrm "if" <> space
                                                                 <> y <> lnbk
                                                                 <> z
                                                                 <> mathrm "," <> space <> mathrm "otherwise"
                                                               )
                                                               (hsExprToLatex (unLoc exprThen))
                                                               (hsExprToLatex (unLoc exprCond))
                                                               (hsExprToLatex (unLoc exprElse))
    hsExprToLatex expr                                = Right
                                                      $ outputableToString expr

-- Pretty print an Outputable value, normally useful when there's no need to
-- parse an Haskell AST constructor down to text level. This function just
-- pretty prints it and passes it through a map symbol filter
outputableToString :: Outputable a => a -> LaTeX
outputableToString = mapSymbol . raw . Text.pack . showSDocUnsafe . ppr

-- | Run Parser
--
-- Runs a parser as described in: https://hackage.haskell.org/package/ghc-lib-parser-9.4.3.20221104/docs/GHC-Parser.html
--
runParser :: FilePath -> String -> P a -> ParseResult a
runParser filename str parser = unP parser parserState
  where
    parserState = initParserState parserOpts b location
    b = stringToStringBuffer str
    location = mkRealSrcLoc (mkFastString filename) 1 1

    parserOpts :: ParserOpts
    parserOpts = mkParserOpts empty empty False True True True

    dynFlags :: DynFlags
    dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig


---- Convert ghc types to ghc-lib-parser types ----

uniqueToUnique :: GHC.Unique -> Unique
uniqueToUnique = uncurry mkUnique . GHC.unpkUnique

fastStringToFastString :: GHC.FastString -> FastString
fastStringToFastString = mkFastString . GHC.unpackFS

occNameToOccName :: GHC.OccName -> OccName
occNameToOccName = mkVarOccFS
                 . fastStringToFastString
                 . GHC.occNameFS

srcLocToSrcLoc :: GHC.SrcLoc -> SrcLoc
srcLocToSrcLoc (GHC.RealSrcLoc rs _) = mkSrcLoc (fastStringToFastString $ GHC.srcLocFile rs) (GHC.srcLocLine rs) (GHC.srcLocCol rs)
srcLocToSrcLoc (GHC.UnhelpfulLoc fs) = mkGeneralSrcLoc (fastStringToFastString fs)

nameToName :: GHC.Name -> Name
nameToName n = mkInternalName (uniqueToUnique (GHC.nameUnique n))
                              (occNameToOccName (GHC.nameOccName n))
                              (mkSrcSpan (srcLocToSrcLoc $ GHC.srcSpanStart (GHC.nameSrcSpan n))
                                         (srcLocToSrcLoc $ GHC.srcSpanEnd (GHC.nameSrcSpan n)))
