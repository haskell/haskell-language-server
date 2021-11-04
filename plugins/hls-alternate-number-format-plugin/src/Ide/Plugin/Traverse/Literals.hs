{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide.Plugin.Traverse.Literals where
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Development.IDE.GHC.Compat hiding (text)
import           Development.IDE.GHC.Util   (unsafePrintSDoc)

-- Depending on our traversal path sometimes we have to parse the "body" of an expression or AST node
-- in the context of how we got there. I.E. we could be in "CMD Mode" where we are parsing a HsCmd or
-- in "Expr Mode" where it's an actual expression
class Traverse a where
    traverseTree :: a -> [Text]

instance Traverse (LHsExpr GhcPs) where
    traverseTree = traverseLExpr

instance Traverse (LHsCmd GhcPs) where
    traverseTree = traverseLCmd

data Literal = UnLoc (HsExpr GhcPs)
             | Loc (LHsExpr GhcPs)

-- | Find all literals in a Parsed Source File
collectLiterals :: ParsedModule -> [Text]
collectLiterals = concatMap traverseLDecl . hsmodDecls . unLoc . pm_parsed_source

collectTcLiterals :: LHsBinds GhcPs -> [Text]
collectTcLiterals = concatMap traverseLBind

-- must attach GhcPs as further down certain located AST nodes are tagged with XRec
-- GhcPs allows us to "unwrap" this XRec type family
-- | Find all Literals in a Declaration.
traverseLDecl :: LHsDecl GhcPs -> [Text]
traverseLDecl = traverseDecl . unLoc

traverseDecl :: HsDecl GhcPs -> [Text]
traverseDecl = \case
  TyClD _ tcd          -> traverseTyClDecl tcd
  ValD _ hsBind        -> traverseBind hsBind
  SpliceD _ spliceDecl -> traverseSpliceDecl spliceDecl
  -- don't think this is necessary
  AnnD _ annDecl       -> traverseAnnDecl annDecl
  RuleD _ ruleDecl     -> traverseRuleDecls ruleDecl
  --------------------------------------------------
  _                    -> []

traverseAnnDecl :: AnnDecl GhcPs -> [Text]
traverseAnnDecl = \case
  HsAnnotation _ _ _ expr -> traverseLExpr expr
  _                       -> []

traverseTyClDecl :: TyClDecl GhcPs -> [Text]
traverseTyClDecl = \case
  ClassDecl{tcdMeths} -> concatMap traverseLBind tcdMeths
  _                   -> []

traverseSpliceDecl :: SpliceDecl GhcPs -> [Text]
traverseSpliceDecl = \case
  SpliceDecl _ splice _ -> traverseLSplice splice
  _                     -> []

traverseRuleDecls :: RuleDecls GhcPs -> [Text]
traverseRuleDecls = \case
  HsRules _ _ ruleDecls -> concatMap traverseLRuleDecl ruleDecls
  _                     -> []

traverseLRuleDecl :: LRuleDecl GhcPs -> [Text]
traverseLRuleDecl = traverseRuleDecl . unLoc

traverseRuleDecl :: RuleDecl GhcPs -> [Text]
traverseRuleDecl = \case
  HsRule{..} -> concatMap traverseLExpr [rd_lhs, rd_rhs]
  _          -> []

traverseLBind :: LHsBindLR idL GhcPs -> [Text]
traverseLBind = traverseBind . unLoc

traverseBind :: HsBindLR idL GhcPs -> [Text]
traverseBind = \case
    FunBind{fun_matches} -> traverseMatchGroup fun_matches
    VarBind{var_rhs}     -> traverseLExpr var_rhs
    _                    -> []

traverseMatchGroup :: Traverse p => MatchGroup GhcPs p -> [Text]
traverseMatchGroup = \case
  MG _ matches _ -> concatMap traverseLMatch $ unLoc matches
  _              -> []

traverseLExpr :: LHsExpr GhcPs -> [Text]
traverseLExpr = traverseExpr . unLoc

traverseExpr :: HsExpr GhcPs -> [Text]
-- traverseExpr self@(L _ hsExpr) = case hsExpr of
traverseExpr = \case
      HsOverLit _ overLit             -> [overLitToString overLit] -- ["OVERLOADED LITERAL" | isNumericOverLit overLit]
      HsLit _ lit                     -> [literalToString lit] -- ["LITERAL" | isNumericLiteral lit]
      HsLam _ group                   -> traverseMatchGroup group
      HsLamCase _ group               -> traverseMatchGroup group
      HsApp _ expr1 expr2             -> concatMap traverseLExpr [expr1, expr2]
      HsAppType _ expr _              -> traverseLExpr expr
      OpApp _ expr1 expr2 expr3       -> concatMap traverseLExpr [expr1, expr2, expr3]
      NegApp _ expr _                 -> traverseLExpr expr
      HsPar _ expr                    -> traverseLExpr expr
      SectionL _ expr1 expr2          -> concatMap traverseLExpr [expr1, expr2]
      SectionR _ expr1 expr2          -> concatMap traverseLExpr [expr1, expr2]
      ExplicitTuple _ args _          -> concatMap traverseLTuple args
      ExplicitSum _ _ _ expr          -> traverseLExpr expr
      HsCase _ expr group             -> traverseLExpr expr <> traverseMatchGroup group
      HsIf _ sexpr expr1 expr2 expr3  -> concatMap traverseLExpr [expr1, expr2, expr3] <> maybe [] traverseSynExpr sexpr
      HsMultiIf _ grhss               -> concatMap traverseLGRHS grhss
      HsLet _ locBinds expr           -> traverseLLocalBinds locBinds <> traverseLExpr expr
      HsDo _ _ stmt                   -> concatMap traverseLStmt (unLoc stmt)
      ExplicitList _ sexpr exprs      -> concatMap traverseLExpr exprs <> maybe [] traverseSynExpr sexpr
      RecordCon{rcon_flds}            -> traverseRecordBinds rcon_flds
      RecordUpd{..}                   -> traverseLExpr rupd_expr <> concatMap traverseLRecordUpdate rupd_flds
      ExprWithTySig _ expr _          -> traverseLExpr expr
      ArithSeq _ sexpr seqInfo        -> maybe [] traverseSynExpr sexpr <> traverseArithSeqInfo seqInfo
      HsSCC _ _ _ expr                -> traverseLExpr expr
      HsCoreAnn _ _ _ expr            -> traverseLExpr expr
      HsBracket _ brackets            -> traverseBrackets brackets
      HsSpliceE _ splice              -> traverseSplice splice
      HsProc _ _ cmdTop               -> traverseLCmdTop cmdTop
      HsStatic _ expr                 -> traverseLExpr expr
      HsTick _ _ expr                 -> traverseLExpr expr
      HsBinTick _ _ _ expr            -> traverseLExpr expr
      HsTickPragma _ _ _ _ expr       -> traverseLExpr expr
      HsWrap _ _ expr                 -> traverseExpr expr
      _                               -> []

traverseLCmdTop :: LHsCmdTop GhcPs -> [Text]
traverseLCmdTop = traverseCmdTop . unLoc

traverseCmdTop :: HsCmdTop GhcPs -> [Text]
traverseCmdTop = \case
  HsCmdTop _ cmds -> traverseLCmd cmds
  _               -> []

traverseLCmd :: LHsCmd GhcPs -> [Text]
traverseLCmd = traverseCmd . unLoc

traverseCmd :: HsCmd GhcPs -> [Text]
traverseCmd = \case
  HsCmdArrApp _ expr1 expr2 _ _   -> concatMap traverseLExpr [expr1, expr2]
  HsCmdArrForm _ expr _ _ cmdTops -> traverseLExpr expr <> concatMap traverseLCmdTop cmdTops
  HsCmdApp _ cmd expr             -> traverseLCmd cmd <> traverseLExpr expr
  HsCmdLam _ group                -> traverseMatchGroup group
  HsCmdPar _ cmd                  -> traverseLCmd cmd
  HsCmdCase _ expr group          -> traverseTree expr <> traverseMatchGroup group
  HsCmdIf _ sexpr expr cmd1 cmd2  -> maybe [] traverseSynExpr sexpr <> traverseLExpr expr <> concatMap traverseLCmd [cmd1, cmd2]
  HsCmdLet _ locBinds cmd         -> traverseLLocalBinds locBinds <> traverseLCmd cmd
  HsCmdDo _ cmdStmts              -> concatMap traverseLStmt (unLoc cmdStmts)
  HsCmdWrap _ _ cmd               -> traverseCmd cmd
  _ -> []

traverseBrackets :: HsBracket GhcPs -> [Text]
traverseBrackets = \case
  ExpBr _ expr     -> traverseLExpr expr
  PatBr _ lpat     -> traverseLPat lpat
  DecBrL _ decls   -> concatMap traverseLDecl decls
  DecBrG _ hsGroup -> traverseHsGroup hsGroup
  TExpBr _ expr    -> traverseLExpr expr
  _                -> []


traverseHsGroup :: HsGroup GhcPs -> [Text]
traverseHsGroup = \case
  HsGroup{hs_valds} -> case hs_valds of
    ValBinds _ hsBinds _ -> concatMap traverseLBind hsBinds
    _                    -> []
  _           -> []

traverseArithSeqInfo :: ArithSeqInfo GhcPs -> [Text]
traverseArithSeqInfo = \case
  From expr                    -> traverseLExpr expr
  FromThen expr1 expr2         -> concatMap traverseLExpr [expr1, expr2]
  FromTo expr1 expr2           -> concatMap traverseLExpr [expr1, expr2]
  FromThenTo expr1 expr2 expr3 -> concatMap traverseLExpr [expr1, expr2, expr3]

traverseLRecordUpdate :: LHsRecUpdField GhcPs -> [Text]
traverseLRecordUpdate = traverseRecordUpdate . unLoc

traverseRecordUpdate :: HsRecUpdField GhcPs -> [Text]
traverseRecordUpdate HsRecField{..} = traverseLExpr hsRecFieldArg

traverseRecordBinds :: HsRecordBinds GhcPs -> [Text]
traverseRecordBinds HsRecFields{..} = concatMap traverseLRecordField rec_flds

traverseLRecordField :: Traverse p => LHsRecField GhcPs p -> [Text]
traverseLRecordField = traverseRecordField . unLoc

traverseRecordField :: Traverse p => HsRecField GhcPs p -> [Text]
traverseRecordField HsRecField{..} = traverseTree hsRecFieldArg

-- | Traverse LStmt to pull out all Located HsLiteral
traverseLStmt :: Traverse p => LStmt GhcPs p -> [Text]
traverseLStmt = traverseStmt . unLoc

traverseStmt :: Traverse p => Stmt GhcPs p -> [Text]
traverseStmt = \case
  LastStmt _ expr _ sexpr            -> traverseTree expr <> traverseSynExpr sexpr
  BindStmt _ lpat expr sexpr1 sexpr2 -> traverseLPat lpat <> traverseTree expr <> concatMap traverseSynExpr [sexpr1, sexpr2]
  ApplicativeStmt _ appPairs sexpr   -> concatMap traverseAppPair appPairs <> maybe [] traverseSynExpr sexpr
  BodyStmt _ expr sexpr1 sexpr2      -> traverseTree expr <> traverseSynExpr sexpr1 <> traverseSynExpr sexpr2
  LetStmt _ locBinds                 -> traverseLLocalBinds locBinds
  ParStmt _ parStmts expr sexpr      -> concatMap traverseParStmtBlock parStmts <> traverseExpr expr <> traverseSynExpr sexpr
  TransStmt{..}                      -> concatMap traverseLStmt trS_stmts <> traverseLExpr trS_using
                                        <> maybe [] traverseLExpr trS_by <> traverseExpr trS_fmap
                                        <> concatMap traverseSynExpr [trS_ret, trS_bind]
  RecStmt{..}                        -> concatMap traverseLStmt recS_stmts
                                        <> concatMap traverseSynExpr [recS_bind_fn, recS_ret_fn, recS_mfix_fn]
  _                                  -> []

traverseParStmtBlock :: ParStmtBlock GhcPs GhcPs -> [Text]
traverseParStmtBlock = \case
  ParStmtBlock _ stmts _ sexpr -> concatMap traverseLStmt stmts <> traverseSynExpr sexpr
  _ -> []

traverseAppPair :: (SyntaxExpr GhcPs, ApplicativeArg GhcPs) -> [Text]
traverseAppPair (sexpr, appArg) = traverseSynExpr sexpr <> traverseAppArg appArg

-- | Traverse ApplicativeArgs to pull out all Located HsLiteral
traverseAppArg :: ApplicativeArg GhcPs -> [Text]
traverseAppArg = \case
  ApplicativeArgOne{..}-> traverseLPat app_arg_pattern <> traverseLExpr arg_expr <> traverseSynExpr fail_operator
  ApplicativeArgMany{..}-> concatMap traverseLStmt app_stmts <> traverseExpr final_expr <> traverseLPat bv_pattern
  _ -> []

-- | Traverse Tuple Arguments to pull out all located HsLiterals
traverseLTuple :: LHsTupArg GhcPs -> [Text]
traverseLTuple = traverseTuple . unLoc

traverseTuple :: HsTupArg GhcPs -> [Text]
traverseTuple = \case
  Present _ expr -> traverseLExpr expr
  _              -> []

-- | Traverse a Match to pull out all Located HsLiterals
traverseLMatch :: Traverse p => LMatch GhcPs p -> [Text]
traverseLMatch = traverseMatch . unLoc

traverseMatch :: Traverse p => Match GhcPs p -> [Text]
traverseMatch = \case
  Match{..} -> concatMap traverseLPat m_pats <> traverseGRHSs m_grhss
  _         -> []

-- | Traverses a Pattern pulling out all Located HsLiterals
traverseLPat :: LPat GhcPs -> [Text]
traverseLPat = traversePat . unLoc

traversePat :: Pat GhcPs -> [Text]
-- traversePat (L _ pat) = case pat of
traversePat = \case
      LazyPat _ lpat                         -> traverseLPat lpat
      AsPat _ _ lpat                         -> traverseLPat lpat
      ParPat _ lpat                          -> traverseLPat lpat
      -- TODO: Do we bother with patterns like this? BangPatterns will never have literals
      BangPat _ lpat                         -> traverseLPat lpat
      ListPat _ lpats                        -> concatMap traverseLPat lpats
      TuplePat _ lpats _                     -> concatMap traverseLPat lpats
      SumPat _ lpat _ _                      -> traverseLPat lpat
      -- ConPat replaced these two in at least ghc-9.2.1 lib
      ConPatIn _ details                     -> concatMap traverseLPat ( hsConPatArgs details)
      ConPatOut{pat_args}                    -> concatMap traverseLPat ( hsConPatArgs pat_args)
      -------------------------------------------------------------------
      ViewPat _ expr lpat                    -> traverseLExpr expr <> traverseLPat lpat
      SplicePat _ splice                     -> traverseSplice splice
      LitPat _ lit                           -> ["LITERAL - LITPAT", literalToString lit] -- ["NUMERIC LITERAL - LITPAT" | isNumericLiteral lit]
    --   NPat _ overLit sexpr1 sexpr2           -> ["OVERLOADED LIT - NPAT" | isNumericOverLit $ unLoc overLit] <> maybe [] traverseSynExpr sexpr1 <> traverseSynExpr sexpr2
      NPat _ overLit sexpr1 sexpr2           -> ["OVERLOADED LIT - NPAT", overLitToString $ unLoc overLit] <> maybe [] traverseSynExpr sexpr1 <> traverseSynExpr sexpr2
    --   NPlusKPat _ _ lOverLit overLit sexpr1 sexpr2 -> ["LOC OVERLOADED LIT ONE - NPLUSKPAT" | isNumericOverLit $ unLoc lOverLit ]
    --                                                   <> ["OVERLOADED LIT TWO - NPLUSKPAT" | isNumericOverLit overLit]
    --                                                   <> traverseSynExpr sexpr1
    --                                                   <> traverseSynExpr sexpr2
      NPlusKPat _ _ lOverLit overLit sexpr1 sexpr2 -> ["LOC OVERLOADED LIT ONE - NPLUSKPAT", overLitToString $ unLoc lOverLit ]
                                                      <> ["OVERLOADED LIT TWO - NPLUSKPAT", overLitToString overLit]
                                                      <> traverseSynExpr sexpr1
                                                      <> traverseSynExpr sexpr2
      SigPat _ lpat _                        -> traverseLPat lpat
      CoPat _ _ pat _                        -> traversePat pat
      _                                      -> []


traverseLSplice :: Located (HsSplice GhcPs) -> [Text]
traverseLSplice = traverseSplice . unLoc

-- | Traverse a Splice to pull out Located HsLiterals
traverseSplice :: HsSplice GhcPs -> [Text]
traverseSplice = \case
  HsTypedSplice _ _ _ expr   -> traverseLExpr expr
  HsUntypedSplice _ _ _ expr -> traverseLExpr expr
  HsSpliced _  _ st          -> case st of
    HsSplicedExpr expr -> traverseExpr expr
    HsSplicedPat pat   -> traversePat pat
    _                  ->[]
  _                          -> []

-- | Traverse a List of GRHS to pull out Located HsLiterals
traverseGRHSs :: Traverse p => GRHSs GhcPs p -> [Text]
traverseGRHSs = \case
  GRHSs{..} -> concatMap traverseLGRHS grhssGRHSs <> traverseLLocalBinds grhssLocalBinds
  _         -> []

-- | Traverse a GRHS to pull out Located HsLiterals
traverseLGRHS :: Traverse p => LGRHS GhcPs p -> [Text]
traverseLGRHS = traverseGRHS . unLoc

traverseGRHS :: Traverse p => GRHS GhcPs p -> [Text]
traverseGRHS = \case
  GRHS _ guards rhs -> traverseTree rhs <> concatMap traverseLStmt guards
  _                 -> []

traverseLLocalBinds :: LHsLocalBinds GhcPs -> [Text]
traverseLLocalBinds = traverseLocalBinds . unLoc

traverseLocalBinds :: HsLocalBinds GhcPs -> [Text]
traverseLocalBinds = \case
  HsValBinds _ valBinds -> traverseValBinds valBinds
  _                     -> []

traverseValBinds :: HsValBindsLR idL GhcPs  -> [Text]
traverseValBinds = \case
  ValBinds _ hsBinds _ -> concatMap traverseLBind hsBinds
  _                    -> []

traverseSynExpr :: SyntaxExpr GhcPs -> [Text]
traverseSynExpr SyntaxExpr{syn_expr} = traverseExpr syn_expr

-- Ignore non-numerics
isNumericLiteral :: HsLit p -> Bool
isNumericLiteral = \case
    HsChar _ _       -> False
    HsCharPrim _ _   -> False
    HsString _ _     -> False
    HsStringPrim _ _ -> False
    _                -> True

isNumericOverLit :: HsOverLit p -> Bool
isNumericOverLit = \case
    OverLit{ol_val} -> isNumericOverLit' ol_val
    _               -> False

-- only a single non-numeric literal
isNumericOverLit' :: OverLitVal -> Bool
isNumericOverLit' = \case
    HsIsString _ _ -> False
    _              -> True

-- mostly for debugging purposes
literalToString :: HsLit p -> Text
literalToString = \case
  HsChar _ c        -> "Char: " <> text c
  HsCharPrim _ c    -> "CharPrim: " <> text c
  HsString _ fs     -> "String: " <> text fs
  HsStringPrim _ bs -> "StringPrim: " <> text bs
  HsInt _ il        -> "Int: " <> text il
  HsIntPrim _ n     -> "IntPrim: " <> text n
  HsWordPrim _ n    -> "WordPrim: " <> text n
  HsInt64Prim _ n   -> "Int64Prim: " <> text n
  HsWord64Prim _ n  -> "Word64Prim: " <> text n
  HsInteger _ n ty  -> "Integer: " <> text n <> " Type: " <> tyToText ty
  HsRat _ fl ty     -> "Rat: " <> text fl <> " Type: " <> tyToText ty
  HsFloatPrim _ fl  -> "FloatPrim: " <> text fl
  HsDoublePrim _ fl -> "DoublePrim: " <>  text fl
  _                 -> "XHsLit"
  where
    text :: Show a => a -> Text
    text = T.pack . show

    tyToText :: Type -> Text
    tyToText = T.pack . unsafePrintSDoc .  ppr

overLitToString :: HsOverLit GhcPs -> Text
overLitToString = \case
  OverLit _ olv he -> (T.concat $ traverseExpr he) <> (case olv of
     HsIntegral il -> case il of { IL st b n -> "IntegralOverLit: " <> text n }
     HsFractional fl -> case fl of { FL st b ra -> "RationalOverLit: " <> text ra }
     HsIsString st fs -> "HIsString: " <> text fs)
  _ -> "XOverLit"
  where
    text :: Show a => a -> Text
    text = T.pack . show
