module Main where

main :: IO ()
main = do
    input <- readFile "inputs/day-01.txt"
    putStrLn "Part 1:"
    print $ part1 input
    putStrLn "Part 2:"
    print $ part2 input

part1 :: String -> Int
part1 _ = 0  -- TODO: implement

part2 :: String -> Int
part2 _ = 0  -- TODO: implement
