module DataStructures.Queue where

import Data.Array.ST
import Control.Monad.ST
import Data.STRef

-- Queue data structure represented using two indices (front, rear) and an array
data Queue s a = Queue {
    front :: STRef s Int,
    rear :: STRef s Int,
    array :: STArray s Int (Maybe a)
}

-- Initialize a new queue with a given size
newQueue :: Int -> ST s (Queue s a)
newQueue size = do
    front <- newSTRef 0
    rear <- newSTRef 0
    array <- newArray (0, size-1) Nothing
    return $ Queue front rear array

-- Enqueue an element
enqueue :: Queue s a -> a -> ST s ()
enqueue q x = do
    r <- readSTRef (rear q)
    writeArray (array q) r (Just x)
    writeSTRef (rear q) (r + 1)

-- Dequeue an element
dequeue :: Queue s a -> ST s (Maybe a)
dequeue q = do
    f <- readSTRef (front q)
    r <- readSTRef (rear q)
    if f == r
        then return Nothing -- Queue is empty
        else do
            x <- readArray (array q) f
            writeSTRef (front q) (f + 1)
            return x

-- Check if the queue is empty
isEmptyQueue :: Queue s a -> ST s Bool
isEmptyQueue q = do
    f <- readSTRef (front q)
    r <- readSTRef (rear q)
    return (f == r)

-- Testing function
testQueue :: [a] -> ([Maybe a], Bool, Bool)
testQueue xs = runST $ do
    queue <- newQueue (length xs)
    mapM_ (enqueue queue) xs
    emptyBefore <- isEmptyQueue queue
    result <- mapM (\_ -> dequeue queue) xs
    emptyAfter <- isEmptyQueue queue
    return (result, emptyBefore, emptyAfter)
