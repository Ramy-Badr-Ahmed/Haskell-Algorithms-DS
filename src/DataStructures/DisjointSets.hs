module DataStructures.DisjointSets where

import Data.Array.ST
import Control.Monad.ST
import Data.STRef

-- Disjoint Set Node represented as an index in an array
type Node = Int

-- Union-Find structure
type DisjointSet s = (STArray s Node Node, STArray s Node Int)

-- Initialize the disjoint set with each node being its own parent and rank zero
makeSet :: Int -> ST s (DisjointSet s)
makeSet n = do
    parentArray <- newListArray (0, n-1) [0..n-1]
    rankArray <- newListArray (0, n-1) (replicate n 0)
    return (parentArray, rankArray)

-- Find with path compression
findSet :: DisjointSet s -> Node -> ST s Node
findSet (parentArray, rankArray) x = do
    parent <- readArray parentArray x
    if parent == x
        then return x
        else do
            root <- findSet (parentArray, rankArray) parent
            writeArray parentArray x root
            return root

-- Union by rank
unionSet :: DisjointSet s -> Node -> Node -> ST s ()
unionSet (parentArray, rankArray) x y = do
    rootX <- findSet (parentArray, rankArray) x
    rootY <- findSet (parentArray, rankArray) y
    if rootX /= rootY
        then do
            rankX <- readArray rankArray rootX
            rankY <- readArray rankArray rootY
            if rankX > rankY
                then writeArray parentArray rootY rootX
                else if rankX < rankY
                    then writeArray parentArray rootX rootY
                    else do
                        writeArray parentArray rootY rootX
                        writeArray rankArray rootY (rankY + 1)
        else return ()

-- Example usage
example :: Int -> [(Node, Node)] -> [Node] -> [Node]
example n unions finds = runST $ do
    ds <- makeSet n
    mapM_ (uncurry $ unionSet ds) unions
    mapM (findSet ds) finds