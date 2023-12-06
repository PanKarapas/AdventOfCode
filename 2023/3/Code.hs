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
        putStrLn ("Part 1: " ++ show (sum (map sum (getPartNumbers (lines contents)))))
        putStrLn ("Part 2: " ++ show (sum (getGearRations (lines contents))))
        hClose handle

---- Part 1 ----
numberLength :: Maybe Int -> Int
numberLength (Just a) = length (show a)
numberLength Nothing = 0

getPartNumbers :: [String] -> [[Int]]
getPartNumbers [] = []
getPartNumbers lines = do
    let paddedLines = "" : lines ++ [""]
    [
        getLinePartNumbers (paddedLines !! (lineIndex-1)) (paddedLines !! lineIndex) (paddedLines !! (lineIndex+1)) 0 |
        lineIndex <- [1.. length lines]
        ]

getLinePartNumbers :: String -> String -> String -> Int -> [Int]
getLinePartNumbers topLine currentLine bottomLine currentIndex
    | currentIndex >= length currentLine = []
    | not (isDigit (currentLine !! currentIndex)) =  getLinePartNumbers topLine currentLine bottomLine (currentIndex + 1)
    | not (anySymbolsAround topLine currentLine bottomLine currentIndex) =
        getLinePartNumbers topLine currentLine bottomLine (currentIndex + 1)
    | otherwise = do
        let partNumber = getNumberWithDigitInIndex currentLine currentIndex
        maybeToList partNumber ++ getLinePartNumbers topLine currentLine bottomLine (currentIndex + length (getIntPrefix (subArray currentIndex (length currentLine) currentLine)))

anySymbolsAround :: String -> String -> String -> Int -> Bool
anySymbolsAround topLine currentLine bottomLine index =
    or [
        isSymbolRelative topLine currentLine bottomLine relativeX relativeY index |
        relativeX <- [-1..1],
        relativeY <- [-1..1],
        relativeX /= 0 || relativeY /= 0
        ]

isSymbolRelative :: String -> String -> String -> Int -> Int -> Int -> Bool
isSymbolRelative topLine currentLine bottomLine relativeX relativeY index
    | index + relativeX < 0 = False
    | index + relativeX < length topLine && relativeY > 0 = isSymbol (topLine !! (index+relativeX))
    | index + relativeX < length bottomLine && relativeY < 0 = isSymbol (bottomLine !! (index+relativeX))
    | index + relativeX < length currentLine = isSymbol (currentLine !! (index+relativeX))
    | otherwise = False

isSymbol :: Char -> Bool
isSymbol x = x /= '.' && not (isDigit x)

---- Part 2 ----

getGearRations :: [String] -> [Int]
getGearRations [] = []
getGearRations lines = do
    let gearIndexes = map (`getLineGearIndexes` 0) lines  
    let paddedLines = "" : lines ++ [""]
    map product (filter (\x -> length x == 2) 
        [
        getAllNumbersAroundIndex (paddedLines !! (lineIndex-1)) (paddedLines !! lineIndex) (paddedLines !! (lineIndex+1)) (gearIndexes !! (lineIndex-1) !! gearIndexesIndex) |
        lineIndex <- [1.. length lines],
        gearIndexesIndex <- [0..length (gearIndexes !! (lineIndex-1))-1]
        ])

getLineGearIndexes :: String -> Int -> [Int]
getLineGearIndexes []  _ = []
getLineGearIndexes (x:xs) index
    | x == '*' = index : getLineGearIndexes xs (index+1) 
    | otherwise = getLineGearIndexes xs (index+1)

getAllNumbersAroundIndex :: String -> String -> String -> Int -> [Int]
getAllNumbersAroundIndex topLine currentLine bottomLine index =
    catMaybes [
        getNumberRelative topLine currentLine bottomLine relativeX relativeY index |
        relativeX <- [-1..1],
        relativeY <- [-1..1],
        relativeX /= 0 || relativeY /= 0,
        relativeX == -1
        || relativeY == 0
        || (relativeX == 0 && relativeY == 1 && index - 1 < length topLine && not (isDigit (topLine !! (index - 1)))) 
        || (relativeX == 1 && relativeY == 1 && index < length topLine && not (isDigit (topLine !! index))) 
        || (relativeX == 0 && relativeY == -1 && index - 1 < length bottomLine && not (isDigit (bottomLine !! (index - 1)))) 
        || (relativeX == 1 && relativeY == -1 && index < length bottomLine && not (isDigit (bottomLine !! index))) 
        ]

getNumberRelative :: String -> String -> String -> Int -> Int -> Int -> Maybe Int
getNumberRelative topLine currentLine bottomLine relativeX relativeY index
    | index + relativeX < 0 = Nothing
    | index + relativeX < length bottomLine && relativeY < 0 = getNumberWithDigitInIndex bottomLine (index+relativeX)
    | index + relativeX < length topLine && relativeY > 0 = getNumberWithDigitInIndex topLine (index+relativeX)
    | index + relativeX < length currentLine = getNumberWithDigitInIndex currentLine (index+relativeX)
    | otherwise = Nothing

---- Common ----
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
    | x == d = acc : splitBy' d xs ""
    |otherwise = splitBy' d xs (acc ++ [x])


getIntPrefix :: String -> String
getIntPrefix xs = getIntPrefix' xs ""

getIntPrefix' :: String -> String -> String
getIntPrefix' [] _ = ""
getIntPrefix' (x:xs) runningTotal
    | not (isDigit x) = runningTotal
    | null xs =  runningTotal ++ [x]
    | otherwise = getIntPrefix' xs (runningTotal ++ [x])

-- Gets the positive int that is in the string at position index
-- The index can point at any of the number's digits
getNumberWithDigitInIndex :: String -> Int -> Maybe Int
getNumberWithDigitInIndex str index
    | index < 0 = Nothing
    | not (isDigit (str !! index)) = Nothing
    | otherwise = do
        let numberToTheLeft = reverse (getIntPrefix (reverse (subArray 0 index str)))
        let numberToTheRight = getIntPrefix (subArray index (length str) str)
        if numberToTheLeft /= "1-" then Just (read (numberToTheLeft ++ numberToTheRight))
        else Just (read numberToTheRight)