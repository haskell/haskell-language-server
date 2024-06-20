{-# LANGUAGE CPP #-}

-- | Compat module Interface file relevant code.
module Development.IDE.GHC.Compat.CmdLine (
          processCmdLineP
        , CmdLineP (..)
        , getCmdLineState
        , putCmdLineState
        , Flag(..)
        , OptKind(..)
        , EwM
        , defFlag
        , liftEwM
    ) where

import           GHC.Driver.CmdLine
import           GHC.Driver.Session (CmdLineP (..), getCmdLineState,
                                     processCmdLineP, putCmdLineState)

