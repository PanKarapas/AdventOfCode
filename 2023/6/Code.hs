{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
import GHC.Float.RealFracMethods (ceilingFloatInt)
import Data.Text.Internal.Fusion.Size (upperBound)

main :: IO ()
main = do
        startTime <- getCurrentTime
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let times = getNumbers (head (lines contents))
        let records = getNumbers (lines contents !! 1)

        putStrLn ("Part 1: " ++ show (product ([length (getTimesThatBeatRecord (times !! i) (records !! i)) | i <- [0 .. length times - 1]])))
        putStrLn ("Part 2: " ++ show (records))
        endTime <- getCurrentTime
        putStrLn ("Execution time: " ++ show (diffUTCTime endTime startTime))
        hClose handle

---- Part 1 ----

getTimesThatBeatRecord :: Int -> Int -> [Int]
getTimesThatBeatRecord totalTime record = do
    let lowerBound = ceiling ((fromIntegral (negate totalTime) + sqrt (fromIntegral (totalTime * totalTime - 4*record)))/fromIntegral (negate 2)+0.0001)
    let upperBound = floor ((fromIntegral (negate totalTime) - sqrt (fromIntegral (totalTime * totalTime - 4*record)))/fromIntegral (negate 2)-0.0001)
    [lowerBound..upperBound]

---- Part 2 ----


---- Common ----

getNumbers :: String -> [Int]
getNumbers [] = []
getNumbers xs = map read (splitBy ' ' (splitBy ':' xs !! 1))

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
