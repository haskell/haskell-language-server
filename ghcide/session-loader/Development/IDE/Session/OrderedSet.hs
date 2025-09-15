module Development.IDE.Session.OrderedSet where

import           Control.Concurrent.STM        (STM, TQueue, newTQueueIO)
import           Control.Concurrent.STM.TQueue (readTQueue, writeTQueue)
import           Control.Monad                 (when)
import           Data.Hashable                 (Hashable)
import qualified Data.HashSet
import qualified Focus
import qualified ListT                         as LT
import qualified StmContainers.Set             as S
import           StmContainers.Set             (Set)


data OrderedSet a = OrderedSet
  { insertionOrder :: TQueue a
  , elements       :: Set a
  }

-- | Insert an element into the ordered set.
-- If the element is not already present, it is added to both the queue and set.
-- If the element already exists, ignore it
insert :: Hashable a => a -> OrderedSet a -> STM ()
insert a (OrderedSet que s) = do
    (_, inserted) <- S.focus (Focus.testingIfInserts $ Focus.insert ()) a s
    -- if already in the set
    when inserted $ writeTQueue que a

newIO :: Hashable a => IO (OrderedSet a)
newIO = do
    que <- newTQueueIO
    s <- S.newIO
    return (OrderedSet que s)

-- | Read the first element from the queue.
-- If an element is not in the set, it means it has been deleted,
-- so we retry until we find a valid element that exists in the set.
readQueue :: Hashable a => OrderedSet a -> STM a
readQueue rs@(OrderedSet que s) = do
                f <- readTQueue que
                b <- S.lookup f s
                -- retry if no files are left in the queue
                if b then return f else readQueue rs

lookup :: Hashable a => a -> OrderedSet a -> STM Bool
lookup a (OrderedSet _ s) = S.lookup a s

-- | Delete an element from the set.
-- The queue is not modified directly; stale entries are filtered out lazily
-- during reading operations (see 'readQueue').
delete :: Hashable a => a -> OrderedSet a -> STM ()
delete a (OrderedSet _ s) = S.delete a s

toHashSet :: Hashable a => OrderedSet a -> STM (Data.HashSet.HashSet a)
toHashSet (OrderedSet _ s) = Data.HashSet.fromList <$> LT.toList (S.listT s)
