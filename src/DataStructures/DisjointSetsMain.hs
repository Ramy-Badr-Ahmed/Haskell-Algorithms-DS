module Main where

import DataStructures.DisjointSets

main :: IO ()
main = do
    let n = 10
    let unions = [(0, 1), (1, 2), (3, 4), (4, 5), (6, 7), (8, 9), (0, 5), (6, 9)]
    let finds = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    let result = example n unions finds
    print result
