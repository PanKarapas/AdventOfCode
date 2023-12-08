{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad ()
import Data.Char ( isDigit )
import Data.List ( isSuffixOf, intersperse, intercalate, find, elemIndex, findIndex, sort )
import Data.Array ()
import Data.Maybe ( catMaybes, maybeToList, isJust, listToMaybe )
import Debug.Trace (trace)
import Data.Time.Clock.POSIX ( getCurrentTime )
import Data.Time (UTCTime, diffUTCTime)

main :: IO ()
main = do
        startTime <- getCurrentTime
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let seeds = getSeeds (head (lines contents))
        let almanac = getAlmanac contents
        putStrLn ("Part 1: " ++ show (minimum (map (`runThroughConversionMap` almanac) seeds )))
        let seedRanges = seedsToTuples seeds
        putStrLn ("Part 2: " ++ show (minimum (map fst (concat (passNumberRangesThroughMultipleMapLayers seedRanges almanac)))))
        endTime <- getCurrentTime
        putStrLn ("Execution time: " ++ show (diffUTCTime endTime startTime))
        hClose handle

---- Part 1 ----23738621
runThroughConversionMap :: Int -> [[ConversionMapEntry]] -> Int
runThroughConversionMap seed [] = seed
runThroughConversionMap seed (x:xs) = do
    let inRange = find (numberIsInConversionMapEntry seed) x
    runThroughConversionMap (maybe seed (\range -> (seed- sourceRangeStart range)+destinationRangeStart range) inRange) xs
---- Part 2 ----

-- Way 1 "The slow way"
-- would be better for inputs where the input had more maps on each layer and each seed range was passing between more maps per layer
-- On my machine takes about 105s to run with my input, to get final result run as:
--         let seeds = getSeeds (head (lines contents))
--         let almanac = getAlmanac contents
--         let reverseAlmanac = reverse (map (map (\entry -> conversionMapEntry (sourceRangeStart entry) (destinationRangeStart entry) (rangeLength entry))) almanac)
--         let seedRanges = seedsToTuples seeds
--         putStrLn ("Part 2: " ++ show (getSmallestLocationThatMapsToInSeedRange reverseAlmanac seedRanges 0))
getSmallestLocationThatMapsToInSeedRange :: [[ConversionMapEntry]] -> [(Int, Int)] -> Int -> Int
getSmallestLocationThatMapsToInSeedRange reverseAlmanac seedRanges i = do
            let seed = runThroughConversionMap i reverseAlmanac
            if any (numberIsInRange seed) seedRanges then i
            else getSmallestLocationThatMapsToInSeedRange reverseAlmanac seedRanges (i+1)

seedsToTuples :: [Int] -> [(Int, Int)]
seedsToTuples [] = []
seedsToTuples [_] = []
seedsToTuples (x:y:xs) = (x, x + y-1) : seedsToTuples xs

-- Way 2, "The fast way"
-- Runs much faster on the given input, would be super slow if the input didn't ranges that mapped so many values in a row on each layer
-- Works by taking each seed range, passing it through a single layer, then taking its output and using it as the input for the next layer
-- Each seed is not calculated individually, instead as many seeds in a row as possible are trated as one, passing just the starting and ending value through the correct map  

passNumberRangesThroughMultipleMapLayers :: [(Int, Int)] -> [[ConversionMapEntry]] -> [[(Int, Int)]]
passNumberRangesThroughMultipleMapLayers [] _ = []
passNumberRangesThroughMultipleMapLayers (x:xs) maps =
    passNumberRangeThroughMultipleMapLayers x maps : passNumberRangesThroughMultipleMapLayers xs maps

passNumberRangeThroughMultipleMapLayers :: (Int, Int) -> [[ConversionMapEntry]] -> [(Int, Int)]
passNumberRangeThroughMultipleMapLayers nRange [] = [nRange]
passNumberRangeThroughMultipleMapLayers nRange (x:xs) =
    concatMap (`passNumberRangeThroughMultipleMapLayers` xs) (passNumberRangeThroughMapLayer nRange x)

-- Takes a seed range and generates the array of ranges generated when passing it through the given map layer
passNumberRangeThroughMapLayer :: (Int, Int) -> [ConversionMapEntry] -> [(Int, Int)]
passNumberRangeThroughMapLayer (start, end) maps =  do
    let mapStartOfRangeIsIn = find (numberIsInConversionMapEntry start) maps
    let nextRange = getNextMap start maps
    let endOfConsecutiveNumbersInSameRange = do
        let startOfMapStartOfRangeIsIn =  fmap sourceRangeStart mapStartOfRangeIsIn
        let lengthOfMapStartOfRangeIsIn = fmap rangeLength mapStartOfRangeIsIn
        let endOfMapStartOfRangeIsIn =fmap (subtract 1)(fmap (+) startOfMapStartOfRangeIsIn <*> lengthOfMapStartOfRangeIsIn)
        let nextRangeStart = fmap (subtract 1.sourceRangeStart) nextRange
        maybe end (min end) (maybeMin [nextRangeStart, endOfMapStartOfRangeIsIn])
    if endOfConsecutiveNumbersInSameRange >= end then [(passNumberThroughMap start mapStartOfRangeIsIn, passNumberThroughMap end mapStartOfRangeIsIn)]
    else
        (passNumberThroughMap start mapStartOfRangeIsIn, passNumberThroughMap endOfConsecutiveNumbersInSameRange mapStartOfRangeIsIn)
        : passNumberRangeThroughMapLayer (endOfConsecutiveNumbersInSameRange+1, end) maps


maybeMin :: [Maybe Int] -> Maybe Int
maybeMin [] = Nothing
maybeMin [x] = x
maybeMin ((Just x):xs)= Just (maybe x (min x) (maybeMin xs))
maybeMin (Nothing:xs)= maybeMin xs

-- Given a number and a list of maps, gets the map with the smallest source start that is bigger than the number
getNextMap :: Int -> [ConversionMapEntry] -> Maybe ConversionMapEntry
getNextMap n [] = Nothing
getNextMap n [x]
    | n < sourceRangeStart x = Just x
    | otherwise = Nothing
getNextMap n (x:xs)
    | n < sourceRangeStart x = do
        let rest = getNextMap n xs
        if isJust rest then
            if sourceRangeStart x < maybe maxBound sourceRangeStart rest then Just x
            else rest
        else Just x
    | otherwise = getNextMap n xs

passNumberThroughMap :: Int -> Maybe ConversionMapEntry -> Int
passNumberThroughMap number (Just m) = runThroughSingleConversionMap number m
passNumberThroughMap number Nothing =  number

runThroughSingleConversionMap :: Int -> ConversionMapEntry -> Int
runThroughSingleConversionMap n m = (n - sourceRangeStart m)+destinationRangeStart m

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
    show c = show (destinationRangeStart c) ++ " " ++ show (sourceRangeStart c) ++ "-" ++ show (sourceRangeStart c + rangeLength c - 1)

numberIsInConversionMapEntry :: Int -> ConversionMapEntry -> Bool
numberIsInConversionMapEntry n r = numberIsInRange n (sourceRangeStart r, sourceRangeStart r + rangeLength r - 1)

numberIsInRange :: Int -> (Int, Int) -> Bool
numberIsInRange n (start, end) = n >= start && n <= end