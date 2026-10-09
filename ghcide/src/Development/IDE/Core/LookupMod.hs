module Development.IDE.Core.LookupMod (lookupMod, LookupModule) where

import           Control.Monad.Trans.Maybe       (MaybeT (MaybeT))
import           Development.IDE.Core.Shake      (HieDbWriter, IdeAction)
import           Development.IDE.GHC.Compat.Core (ModuleName, Unit)
import           Development.IDE.Types.Location  (Uri)

-- | Gives a Uri for the module, given the .hie file location and the the module info
-- The Bool denotes if it is a boot module
type LookupModule m = FilePath -> ModuleName -> Unit -> Bool -> MaybeT m Uri

-- | Eventually this will lookup/generate URIs for files in dependencies, but not in the
-- project. Right now, this is just a stub.
lookupMod ::
  -- | access the database
  HieDbWriter ->
  -- | The `.hie` file we got from the database
  FilePath ->
  ModuleName ->
  Unit ->
  -- | Is this file a boot file?
  Bool ->
  MaybeT IdeAction Uri
lookupMod _dbchan _hie_f _mod _uid _boot = MaybeT $ pure Nothing
