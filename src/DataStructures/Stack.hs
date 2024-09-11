import Data.Array.ST
import Control.Monad.ST
import Data.STRef

type Stack s a = (STArray s Int a, STRef s Int)

-- Create a new stack
newStack :: Int -> ST s (Stack s a)
newStack size = do
    arr <- newArray_ (0, size - 1)
    topRef <- newSTRef (-1)
    return (arr, topRef)

-- Push an element onto the stack
push :: Stack s a -> a -> ST s ()
push (arr, topRef) x = do
    top <- readSTRef topRef
    let newTop = top + 1
    writeArray arr newTop x
    writeSTRef topRef newTop

-- Pop an element from the stack
pop :: Stack s a -> ST s (Maybe a)
pop (arr, topRef) = do
    top <- readSTRef topRef
    if top < 0
        then return Nothing
        else do
            x <- readArray arr top
            writeSTRef topRef (top - 1)
            return (Just x)

-- Peek at the top element of the stack
peek :: Stack s a -> ST s (Maybe a)
peek (arr, topRef) = do
    top <- readSTRef topRef
    if top < 0
        then return Nothing
        else Just <$> readArray arr top

-- Check if the stack is empty
isEmpty :: Stack s a -> ST s Bool
isEmpty (_, topRef) = do
    top <- readSTRef topRef
    return (top == -1)

-- Example usage and testing function
testStack :: [Int] -> ([Maybe Int], Bool, Bool)
testStack xs = runST $ do
    stack <- newStack (length xs)
    mapM_ (push stack) xs
    emptyBefore <- isEmpty stack
    result <- mapM (\_ -> pop stack) xs
    emptyAfter <- isEmpty stack
    return (result, emptyBefore, emptyAfter)

main :: IO ()
main = do
    let input = [1, 2, 3, 4, 5]
    let (result, emptyBefore, emptyAfter) = testStack input
    print result           -- Expected output: [Just 5, Just 4, Just 3, Just 2, Just 1]
    print emptyBefore      -- Expected output: False
    print emptyAfter       -- Expected output: True
