module Development.IDE.Plugin.CodeAction.Rules
  ( rulePackageExports
  )
where

import           Data.Traversable               ( forM )
import           Development.IDE.Core.Rules
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.CodeAction.RuleTypes
import           Development.IDE.Types.Exports
import           Development.Shake
import           GHC                            ( DynFlags(pkgState) )
import           HscTypes                       ( hsc_dflags)
import           LoadIface
import           Maybes
import           Module                         ( Module(..) )
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

  modIfaces <- forM targets $ \(pkg, mn) -> do
    modIface <- liftIO $ initIfaceLoad env $ loadInterface
      ""
      (Module (packageConfigId pkg) mn)
      (ImportByUser False)
    return $ case modIface of
      Failed    _err -> Nothing
      Succeeded mi   -> Just mi
  return $ createExportsMap (catMaybes modIfaces)
