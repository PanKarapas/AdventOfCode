import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )  
import Control.Monad ()
import Data.Char ( isDigit )
import Data.Maybe

main :: IO ()
main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        putStrLn ("Part 1: " ++ show (fmap sum (mapM processPart1 (lines contents))))
        putStrLn ("Part 2: " ++ show (fmap sum (mapM processPart2 (lines contents))))
        hClose handle   


-- Assumes at least one number exists in the string
processPart1 :: String -> Maybe Int
processPart1 xs = fmap read (fmap (:) (getFirstDigit xs) <*> fmap (:[]) ((getFirstDigit . reverse) xs))

getFirstDigit :: String -> Maybe Char
getFirstDigit [] = Nothing
getFirstDigit (x:xs)
    | isDigit x = Just x
    | otherwise = getFirstDigit xs

processPart2 :: String -> Maybe Int
processPart2 xs = fmap read (fmap (:) (getFirstDigitIncludingStrings xs) <*> fmap (:[]) (getLastDigitIncludingString xs))

getFirstDigitIncludingStrings :: String -> Maybe Char
getFirstDigitIncludingStrings [] = Nothing
getFirstDigitIncludingStrings ('o':'n':'e':xs) = Just '1'
getFirstDigitIncludingStrings ('t':'w':'o':xs) = Just '2'
getFirstDigitIncludingStrings ('t':'h':'r':'e':'e':xs) = Just '3'
getFirstDigitIncludingStrings ('f':'o':'u':'r':xs) = Just '4'
getFirstDigitIncludingStrings ('f':'i':'v':'e':xs) = Just '5'
getFirstDigitIncludingStrings ('s':'i':'x':xs) = Just '6'
getFirstDigitIncludingStrings ('s':'e':'v':'e':'n':xs) = Just '7'
getFirstDigitIncludingStrings ('e':'i':'g':'h':'t':xs) = Just '8'
getFirstDigitIncludingStrings ('n':'i':'n':'e':xs) = Just '9'
getFirstDigitIncludingStrings (x:xs) | isDigit x = Just x | otherwise = getFirstDigit xs

getLastDigitIncludingString :: String -> Maybe Char
getLastDigitIncludingString [] = Nothing
getLastDigitIncludingString xs = head ([getFirstDigitIncludingStrings x | i <- [1..length xs], let x = lastN i xs, isJust (getFirstDigitIncludingStrings x)]  ++ [Nothing])

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs