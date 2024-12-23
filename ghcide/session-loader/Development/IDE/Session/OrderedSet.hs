module Development.IDE.Session.OrderedSet where

import           Control.Concurrent.STM        (STM, TQueue, newTQueueIO)
import           Control.Concurrent.STM.TQueue (readTQueue, writeTQueue)
import           Control.Monad                 (when)
import           Data.Hashable                 (Hashable)
import qualified Focus
import qualified ListT                         as LT
import qualified StmContainers.Set             as S
import           StmContainers.Set             (Set)


type OrderedSet a = (TQueue a, Set a)

insert :: Hashable a => a -> OrderedSet a -> STM ()
insert a (que, s) = do
    (_, inserted) <- S.focus (Focus.testingIfInserts $ Focus.insert ()) a s
    when inserted $ writeTQueue que a

newIO :: Hashable a => IO (OrderedSet a)
newIO = do
    que <- newTQueueIO
    s <- S.newIO
    return (que, s)

readQueue :: Hashable a => OrderedSet a -> STM a
readQueue rs@(que, s) = do
                f <- readTQueue que
                b <- S.lookup f s
                -- retry if the file is already in done
                if b then return f else readQueue rs

lookup :: Hashable a => a -> OrderedSet a -> STM Bool
lookup a (_, s) = S.lookup a s

delete :: Hashable a => a -> OrderedSet a -> STM ()
delete a (_, s) = S.delete a s

toUnOrderedList :: Hashable a => OrderedSet a -> STM [a]
toUnOrderedList (_, s) = LT.toList $ S.listT s
