import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad ()
import Data.Char ( isDigit )
import Data.List ( isSuffixOf )
import Data.Array ()
import Data.Maybe ( catMaybes, maybeToList )

main :: IO ()
main = do
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let lineWiningNumbers = map getLineWinningNumbers (lines contents)
        putStrLn ("Part 1: " ++ show (sum (map getWinningNumbersPoints lineWiningNumbers)))
        putStrLn ("Part 2: " ++ show (sum (getTotalScratchCards lineWiningNumbers)))
        hClose handle

---- Part 1 ----
getWinningNumbersPoints :: [Int] -> Int
getWinningNumbersPoints [] = 0
getWinningNumbersPoints [x] = 1
getWinningNumbersPoints (x:xs) = 2 * getWinningNumbersPoints xs

---- Part 2 ----
-- Takes as input the number of winning numbers on each line and returns the total number of each of the cards
getTotalScratchCards :: [[Int]] -> [Int]
getTotalScratchCards linesWinningNumbers = do
    let winningNumbersLength = [length winningNumbers | winningNumbers <- linesWinningNumbers]
    let x = 1 : [
            sum quantityOfEachCardThatWinsThisOne + 1 |
            index <- [1.. length linesWinningNumbers-1],
            let quantityOfEachCardThatWinsThisOne = do
                [
                    x !! candidateIndex | 
                    let candidates = subArray 0 index winningNumbersLength, 
                    candidateIndex <- [0.. length candidates-1], 
                    candidates !! candidateIndex > length candidates-1-candidateIndex]
                ]
    x


---- Common ----

getLineWinningNumbers :: String -> [Int]
getLineWinningNumbers [] = []
getLineWinningNumbers xs = do
    let splitLine = splitBy '|' (splitBy ':' xs !! 1)
    let winningNumbers = map read (splitBy ' ' (head splitLine)) :: [Int]
    let cardNumbers = map read (splitBy ' ' (splitLine !! 1)) :: [Int]
    intersection winningNumbers cardNumbers

subArray :: Int -> Int -> [a] -> [a]
subArray i j arr = [arr !! index | index <- [i..j-1]]

intersection :: Eq a => [a] -> [a] -> [a]
intersection a = filter (`elem` a)

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