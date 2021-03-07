{-# LANGUAGE TemplateHaskell #-}

module Development.IDE.Plugin.CodeAction.Args.TH (mkInstances) where

import           Language.Haskell.TH

mkInstances :: Name -> DecsQ
mkInstances tyConName =
  reify tyConName >>= \case
    (TyConI (DataD _ _ _ _ [RecC dataConName tys] _)) -> concat <$> mapM (genForVar dataConName) tys
    _ -> error "unsupported"
 where
  clsType = conT $ mkName "ToCodeAction"
  methodName = mkName "toCodeAction"
  tempType = varT $ mkName "r"
  commonFun dataConName fieldName = funD methodName [clause [mkName "caa" `asP` recP dataConName [fieldPat fieldName $ varP (mkName "x")], varP (mkName "f")] (normalB [|$(varE methodName) caa $ f x|]) []]
  genForVar dataConName (fieldName, _, ty@(AppT (ConT _maybe) ty'))
    | _maybe == ''Maybe =
      do
        withMaybe <-
          instanceD
            (cxt [clsType `appT` tempType])
            (clsType `appT` ((arrowT `appT` pure ty) `appT` tempType))
            [commonFun dataConName fieldName]
        withoutMaybe <-
          instanceD
            (cxt [clsType `appT` tempType])
            (clsType `appT` ((arrowT `appT` pure ty') `appT` tempType))
            [ funD
                methodName
                [ clause [mkName "caa" `asP` recP dataConName [fieldPat fieldName $ conP 'Just [varP (mkName "x")]], varP (mkName "f")] (normalB [|$(varE methodName) caa $ f x|]) []
                , clause [wildP, wildP] (normalB [|[]|]) []
                ]
            ]
        pure [withMaybe, withoutMaybe]
  genForVar dataConName (fieldName, _, ty) =
    pure
      <$> instanceD
        (cxt [clsType `appT` tempType])
        (clsType `appT` ((arrowT `appT` pure ty) `appT` tempType))
        [commonFun dataConName fieldName]
