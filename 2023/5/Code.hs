{-# LANGUAGE InstanceSigs #-}
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad ()
import Data.Char ( isDigit )
import Data.List ( isSuffixOf, intersperse, intercalate, find )
import Data.Array ()
import Data.Maybe ( catMaybes, maybeToList, isJust )
import Debug.Trace (trace)


main :: IO ()
main = do
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let seeds = getSeeds (head (lines contents))
        let almanac = getAlmanac contents
        putStrLn ("Part 1: " ++ show (minimum (map (`runThroughConversionMap` almanac) seeds )))
        let reverseAlmanac = reverse (map (map (\entry -> conversionMapEntry (sourceRangeStart entry) (destinationRangeStart entry) (rangeLength entry))) almanac)
        let seedRanges = seedsToRanges seeds
        putStrLn ("Part 2: " ++ show (head [i | i <- [1..], let seed = runThroughConversionMap i reverseAlmanac, any (numberIsInConversionMapEntry seed) seedRanges]))
        hClose handle

---- Part 1 ----23738621
runThroughConversionMap :: Int -> [[ConversionMapEntry]] -> Int
runThroughConversionMap seed [] = seed
runThroughConversionMap seed (x:xs) = do
    let inRange = find (numberIsInConversionMapEntry seed) x
    runThroughConversionMap (maybe seed (\range -> (seed- sourceRangeStart range)+destinationRangeStart range) inRange) xs

---- Part 2 ----
seedsToRanges :: [Int] -> [ConversionMapEntry]
seedsToRanges [] = []
seedsToRanges [_] = []
seedsToRanges (x:y:xs) = conversionMapEntry x x y : seedsToRanges xs

---- Common ----

getSeeds :: String -> [Int]
getSeeds [] = []
getSeeds xs = map read (splitBy ' ' (splitBy ':' xs !! 1))

getAlmanac :: String -> [[ConversionMapEntry]]
getAlmanac contents = map (getAlmanacEntry . tail . lines) (tail (splitBy ':' (intercalate "\n" (tail (tail (lines contents))))))

getAlmanacEntry :: [String] -> [ConversionMapEntry]
getAlmanacEntry [] = []
getAlmanacEntry (x:xs)
    | x == "" = []
    | otherwise =
        conversionMapEntry (head parts) (parts !! 1) (parts !! 2)
        : getAlmanacEntry xs
        where parts = map read (splitBy ' ' x) :: [Int]

subArray :: Int -> Int -> [a] -> [a]
subArray i j arr = [arr !! index | index <- [i..j-1]]

splitBy :: Char -> String -> [String]
splitBy d s = splitBy' d s ""

splitBy' :: Char -> String -> String -> [String]
splitBy' _ [] acc = [acc]
splitBy' d [x] acc
    | x == d = [acc]
    | otherwise = [acc ++ [x]]
splitBy' d (x:xs) acc
    | x == d  && acc /= ""= acc : splitBy' d xs ""
    | x == d = splitBy' d xs ""
    |otherwise = splitBy' d xs (acc ++ [x])

---- Data ----

data ConversionMapEntry = ConversionMapEntry{ destinationRangeStart :: Int, sourceRangeStart :: Int, rangeLength :: Int }
conversionMapEntry :: Int -> Int -> Int -> ConversionMapEntry
conversionMapEntry r g b = ConversionMapEntry {destinationRangeStart = r, sourceRangeStart = g, rangeLength = b}

instance Show ConversionMapEntry where
    show :: ConversionMapEntry -> String
    show c = show (destinationRangeStart c) ++ " " ++ show (sourceRangeStart c) ++ "-" ++ show (sourceRangeStart c + rangeLength c)

numberIsInConversionMapEntry :: Int -> ConversionMapEntry -> Bool
numberIsInConversionMapEntry n r = n >= sourceRangeStart r && n <= sourceRangeStart r + rangeLength r