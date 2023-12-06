{-# LANGUAGE InstanceSigs #-}
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )  
import Control.Monad ()
import Data.Char ( isDigit )
import Data.List ( isSuffixOf )

main :: IO ()
main = do  
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        putStrLn ("Part 1: " ++ show (sum (map getIdIfValid (lines contents))))
        putStrLn ("Part 2: " ++ show (sum (map getGamePower (lines contents))))
        hClose handle

---- Part 1 ----

-- accepts a string in the input format and returns 0 if its invalid or it's  game id if its valid
getIdIfValid :: String -> Int
getIdIfValid [] = 0
getIdIfValid xs = do
    let parts = splitBy ':' xs
    if isGameValid (parts !! 1) then read (splitBy ' ' (head parts) !! 1) :: Int
    else 0

isGameValid :: String -> Bool
isGameValid [] = True
isGameValid xs = all ((\c -> red c <= 12 && green c <= 13 && blue c <= 14) . getSingleCounts) (splitBy ',' xs >>= splitBy ';')

---- Part 2 ----

-- accepts a string in the input format and returns the game's power
getGamePower :: String -> Int
getGamePower [] = 0
getGamePower xs = do
    let inputWithoutGameTag = splitBy ':' xs !! 1
    let max = maxCounts (map getSingleCounts (splitBy ',' inputWithoutGameTag >>= splitBy ';'))
    red max * green max * blue max

-- Gets a count that has the max value for each color of the Counts provided
maxCounts :: [Counts] -> Counts
maxCounts [] = counts 0 0 0
maxCounts [x] = x
maxCounts (x:xs) = do
    let remainingCounts = maxCounts xs
    counts (max (red x) (red remainingCounts)) (max (green x) (green remainingCounts)) (max (blue x) (blue remainingCounts))

---- Common ----

--  " 5 green" -> counts 0 5 0
getSingleCounts :: String -> Counts
getSingleCounts [] = counts 0 0 0
getSingleCounts xs 
    | "red" `isSuffixOf` xs = counts (extractCount xs) 0 0
    | "green" `isSuffixOf` xs = counts 0 (extractCount xs) 0
    | "blue" `isSuffixOf` xs = counts 0 0 (extractCount xs)
    | otherwise = counts 0 0 0

-- " 5 red" -> 5
extractCount :: String -> Int
extractCount xs = read (splitBy ' ' xs !! 1)

splitBy :: Char -> String -> [String]
splitBy d s = splitBy' d s ""

splitBy' :: Char -> String -> String -> [String]
splitBy' _ [] acc = [acc]
splitBy' d [x] acc
    | x == d = [acc]
    | otherwise = [acc ++ [x]]
splitBy' d (x:xs) acc
    | x == d = acc : splitBy' d xs ""
    |otherwise = splitBy' d xs (acc ++ [x])

data Counts = Counts{ red :: Int, green :: Int, blue :: Int }

counts :: Int -> Int -> Int -> Counts
counts r g b = Counts {red = r, green = g, blue = b}

instance Show Counts where
    show :: Counts -> String
    show c = "Red: " ++ show (red c) ++ ", Green: " ++ show (green c) ++ ", Blue: " ++ show (blue c)
