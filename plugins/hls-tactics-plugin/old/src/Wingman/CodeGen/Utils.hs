module Wingman.CodeGen.Utils where

import Data.String
import Data.List
import Development.IDE.GHC.Compat
import GHC.SourceGen (RdrNameStr (UnqualStr), recordConE, string)
import GHC.SourceGen.Overloaded as SourceGen
import Wingman.GHC (getRecordFields)


------------------------------------------------------------------------------
-- | Make a data constructor with the given arguments.
mkCon :: ConLike -> [Type] -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkCon con apps (fmap unLoc -> args)
  | RealDataCon dcon <- con
  , dcon == nilDataCon
  , [ty] <- apps
  , ty `eqType` charTy = noLoc $ string ""

  | RealDataCon dcon <- con
  , isTupleDataCon dcon =
      noLoc $ tuple args

  | RealDataCon dcon <- con
  , dataConIsInfix dcon
  , (lhs : rhs : args') <- args =
      noLoc $ foldl' (@@) (op lhs (coerceName con_name) rhs) args'

  | Just fields <- getRecordFields con
  , length fields >= 2 =  --  record notation is unnatural on single field ctors
      noLoc $ recordConE (coerceName con_name) $ do
        (arg, (field, _)) <- zip args fields
        pure (coerceName field, arg)

  | otherwise =
      noLoc $ foldl' (@@) (bvar' $ occName con_name) args
  where
    con_name = conLikeName con


coerceName :: HasOccName a => a -> RdrNameStr
coerceName = UnqualStr . fromString . occNameString . occName


------------------------------------------------------------------------------
-- | Like 'var', but works over standard GHC 'OccName's.
var' :: SourceGen.Var a => OccName -> a
var' = var . fromString . occNameString


------------------------------------------------------------------------------
-- | Like 'bvar', but works over standard GHC 'OccName's.
bvar' :: BVar a => OccName -> a
bvar' = bvar . fromString . occNameString


------------------------------------------------------------------------------
-- | Get an HsExpr corresponding to a function name.
mkFunc :: String -> HsExpr GhcPs
mkFunc = var' . mkVarOcc


------------------------------------------------------------------------------
-- | Get an HsExpr corresponding to a value name.
mkVal :: String -> HsExpr GhcPs
mkVal = var' . mkVarOcc


------------------------------------------------------------------------------
-- | Like 'op', but easier to call.
infixCall :: String -> HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
infixCall s = flip op (fromString s)


------------------------------------------------------------------------------
-- | Like '(@@)', but uses a dollar instead of parentheses.
appDollar :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
appDollar = infixCall "$"

