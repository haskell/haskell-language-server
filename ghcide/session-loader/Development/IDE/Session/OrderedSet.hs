module Development.IDE.Session.OrderedSet where

import           Control.Concurrent.STM        (STM, TQueue, flushTQueue,
                                                newTQueueIO)
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
    -- if already in the set
    -- update the position of the element in the queue
    when (not inserted) $ do
            items <- filter (==a) <$> flushTQueue que
            mapM_ (writeTQueue que) items
            return ()
    writeTQueue que a
            -- when que $ writeTQueue que a

newIO :: Hashable a => IO (OrderedSet a)
newIO = do
    que <- newTQueueIO
    s <- S.newIO
    return (que, s)

readQueue :: Hashable a => OrderedSet a -> STM a
readQueue rs@(que, s) = do
                f <- readTQueue que
                b <- S.lookup f s
                -- retry if no files are left in the queue
                if b then return f else readQueue rs

lookup :: Hashable a => a -> OrderedSet a -> STM Bool
lookup a (_, s) = S.lookup a s

delete :: Hashable a => a -> OrderedSet a -> STM ()
delete a (_, s) = S.delete a s

toUnOrderedList :: Hashable a => OrderedSet a -> STM [a]
toUnOrderedList (_, s) = LT.toList $ S.listT s
