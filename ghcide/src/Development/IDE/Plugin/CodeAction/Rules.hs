module Development.IDE.Plugin.CodeAction.Rules
  ( rulePackageExports
  )
where

import           Data.HashMap.Strict            ( fromListWith )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Traversable               ( forM )
import           Development.IDE.Core.Rules
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.CodeAction.RuleTypes
import           Development.Shake
import           GHC                            ( DynFlags(pkgState) )
import           HscTypes                       ( IfaceExport
                                                , hsc_dflags
                                                , mi_exports
                                                )
import           LoadIface
import           Maybes
import           Module                         ( Module(..)
                                                , ModuleName
                                                , moduleNameString
                                                )
import           Packages                       ( explicitPackages
                                                , exposedModules
                                                , packageConfigId
                                                )
import           TcRnMonad                      ( WhereFrom(ImportByUser)
                                                , initIfaceLoad
                                                )

rulePackageExports :: Rules ()
rulePackageExports = defineNoFile $ \(PackageExports session) -> do
  let env     = hscEnv session
      pkgst   = pkgState (hsc_dflags env)
      depends = explicitPackages pkgst
      targets =
        [ (pkg, mn)
        | d        <- depends
        , Just pkg <- [lookupPackageConfig d env]
        , (mn, _)  <- exposedModules pkg
        ]

  results <- forM targets $ \(pkg, mn) -> do
    modIface <- liftIO $ initIfaceLoad env $ loadInterface
      ""
      (Module (packageConfigId pkg) mn)
      (ImportByUser False)
    case modIface of
      Failed    _err -> return mempty
      Succeeded mi   -> do
        let avails = mi_exports mi
        return $ concatMap (unpackAvail mn) avails
  return $ fromListWith (++) $ concat results

unpackAvail :: ModuleName -> IfaceExport -> [(Text, [(IdentInfo, Text)])]
unpackAvail mod =
  map (\id@IdentInfo {..} -> (name, [(id, pack $ moduleNameString mod)]))
    . mkIdentInfos
