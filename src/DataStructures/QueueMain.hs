{-|
Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed) in Pull Request: #54
https://github.com/TheAlgorithms/Haskell/pull/54

Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request addressing bugs/corrections to this file.
Thank you!
-}

module DataStructures.QueueMain where

import DataStructures.Queue

main :: IO ()
main = do
    let input = [1, 2, 3, 4, 5]
    let (result, emptyBefore, emptyAfter) = testQueue input
    print result          -- Expected output: [Just 1, Just 2, Just 3, Just 4, Just 5]
    print emptyBefore     -- Expected output: False
    print emptyAfter      -- Expected output: True
