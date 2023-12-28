{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

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

#if MIN_VERSION_ghc(9,3,0)
import GHC.Driver.Session (processCmdLineP, CmdLineP (..), getCmdLineState, putCmdLineState)
import GHC.Driver.CmdLine
#else
import GHC.Driver.CmdLine
import Control.Monad.IO.Class
import GHC (Located)
#endif

#if !MIN_VERSION_ghc(9,3,0)
-- | A helper to parse a set of flags from a list of command-line arguments, handling
-- response files.
processCmdLineP
    :: forall s m. MonadIO m
    => [Flag (CmdLineP s)]  -- ^ valid flags to match against
    -> s                    -- ^ current state
    -> [Located String]     -- ^ arguments to parse
    -> m (([Located String], [Err], [Warn]), s)
                            -- ^ (leftovers, errors, warnings)
processCmdLineP activeFlags s0 args =
    pure $ runCmdLine (processArgs activeFlags args) s0
#endif
