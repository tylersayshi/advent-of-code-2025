module Main where

import System.Directory (listDirectory, createDirectoryIfMissing)
import Text.Printf (printf)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
    -- Find existing day directories
    entries <- listDirectory "."
    let dayDirs = filter ("day-" `isPrefixOf`) entries
        dayNums = mapMaybe extractDayNum dayDirs
        nextDay = if null dayNums then 1 else maximum dayNums + 1
        dayStr = printf "%02d" nextDay :: String
        dayDir = "day-" ++ dayStr
        inputFile = "inputs/day-" ++ dayStr ++ ".txt"
        mainFile = dayDir ++ "/Main.hs"

    putStrLn $ "Creating day " ++ dayStr ++ "..."

    -- Create day directory
    createDirectoryIfMissing True dayDir

    -- Create Main.hs from template
    writeFile mainFile $ mainTemplate dayStr

    -- Create empty input file
    createDirectoryIfMissing True "inputs"
    writeFile inputFile ""

    -- Append to cabal file
    appendToCabal dayStr

    putStrLn $ "Created " ++ dayDir ++ "/Main.hs"
    putStrLn $ "Created " ++ inputFile
    putStrLn $ "Updated advent-of-code2025.cabal"
    putStrLn $ "Run with: cabal run day-" ++ dayStr

extractDayNum :: String -> Maybe Int
extractDayNum s = case drop 4 s of  -- drop "day-"
    numStr -> readMaybe numStr

mainTemplate :: String -> String
mainTemplate dayStr = unlines
    [ "module Main where"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "    input <- readFile \"inputs/day-" ++ dayStr ++ ".txt\""
    , "    putStrLn \"Part 1:\""
    , "    print $ part1 input"
    , "    putStrLn \"Part 2:\""
    , "    print $ part2 input"
    , ""
    , "part1 :: String -> Int"
    , "part1 _ = 0  -- TODO: implement"
    , ""
    , "part2 :: String -> Int"
    , "part2 _ = 0  -- TODO: implement"
    ]

appendToCabal :: String -> IO ()
appendToCabal dayStr = do
    let cabalEntry = unlines
            [ ""
            , "executable day-" ++ dayStr
            , "    import:           warnings"
            , "    main-is:          Main.hs"
            , "    build-depends:    base ^>=4.18.3.0"
            , "    hs-source-dirs:   day-" ++ dayStr
            , "    default-language: Haskell2010"
            ]
    appendFile "advent-of-code2025.cabal" cabalEntry
