module Wingman.CodeGen.Utils where

import Data.List
import DataCon
import Development.IDE.GHC.Compat
import GHC.Exts
import GHC.SourceGen (RdrNameStr, recordConE, string)
import GHC.SourceGen.Overloaded
import GhcPlugins (nilDataCon, charTy, eqType)
import Name
import Wingman.GHC (getRecordFields)


------------------------------------------------------------------------------
-- | Make a data constructor with the given arguments.
mkCon :: DataCon -> [Type] -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkCon dcon apps (fmap unLoc -> args)
  | dcon == nilDataCon
  , [ty] <- apps
  , ty `eqType` charTy = noLoc $ string ""
  | isTupleDataCon dcon =
      noLoc $ tuple args
  | dataConIsInfix dcon
  , (lhs : rhs : args') <- args =
      noLoc $ foldl' (@@) (op lhs (coerceName dcon_name) rhs) args'
  | Just fields <- getRecordFields dcon
  , length fields >= 2 =  --  record notation is unnatural on single field ctors
      noLoc $ recordConE (coerceName dcon_name) $ do
        (arg, (field, _)) <- zip args fields
        pure (coerceName field, arg)
  | otherwise =
      noLoc $ foldl' (@@) (bvar' $ occName dcon_name) args
  where
    dcon_name = dataConName dcon


coerceName :: HasOccName a => a -> RdrNameStr
coerceName = fromString . occNameString . occName


------------------------------------------------------------------------------
-- | Like 'var', but works over standard GHC 'OccName's.
var' :: Var a => OccName -> a
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

