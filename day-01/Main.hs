module Main where

main :: IO ()
main = do
    input <- readFile "inputs/day-01.txt"
    putStrLn "Part 1:"
    result <- part1 input
    print result
    putStrLn "Part 2:"
    result2 <- part2 input
    print result2

data Instruction = Instruction { isLeft :: Bool, amount :: Int }
    deriving Show

parseLine :: String -> Instruction
parseLine line = Instruction (head line == 'L') (read (tail line))

part1 :: String -> IO Int
part1 input = do
    let counter = 50
    let instructions = map parseLine (lines input)
    let (_finalCounter, zeroCount) = foldl updateCounter (counter, 0) instructions
    return zeroCount

updateCounter :: (Int, Int) -> Instruction -> (Int, Int)
updateCounter (counter, zeros) (Instruction left amt) =
    let newCounter = (if left then counter - amt else counter + amt) `mod` 100
        newZeros = if newCounter == 0 then zeros + 1 else zeros
    in (newCounter, newZeros)

part2 :: String -> IO Int
part2 input = do
    let counter = 50
    let instructions = map parseLine (lines input)
    let (_finalCounter, zeroCount) = foldl updateCounter2 (counter, 0) instructions
    return zeroCount

updateCounter2 :: (Int, Int) -> Instruction -> (Int, Int)
updateCounter2 (counter, zeros) (Instruction left amt) =
    let newCounter = (if left then counter - amt else counter + amt) `mod` 100
        passedZeroTimes
            | left = if counter == 0
                     then amt `div` 100  -- starting at 0, click 0 again at steps 100, 200, ...
                     else if amt >= counter
                          then ((amt - counter) `div` 100) + 1  -- click 0 at steps counter, counter+100, ...
                          else 0
            | otherwise = if counter == 0
                          then amt `div` 100  -- starting at 0, click 0 again at steps 100, 200, ...
                          else (counter + amt) `div` 100  -- click 0 when we pass 99->0
        newZeros = zeros + passedZeroTimes
    in (newCounter, newZeros)
