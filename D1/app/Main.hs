module Main where
import Data.Char (isDigit, digitToInt)
import Text.Replace
import GHC.IO.FD (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hClose)

main :: IO ()
main = do
    contents <- readFile "data/data"
    -- putStr . show . simpleTask $ contents
    (print . workOnContents) contents

numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "zero"]

findAndReplaceLetterDigit line digitString = 

takeDigitsFromLine :: [Char] -> [Int]
takeDigitsFromLine = firstAndLast. map digitToInt . filter isDigit

firstAndLast :: [a] -> [a]
firstAndLast xs = head xs:[last xs]

twoDigitsToNumber :: Num a => [a] -> a
twoDigitsToNumber (x:y:_) = 10 * x + y
twoDigitsToNumber _ = undefined

workOnLine :: [Char] -> Int
workOnLine = twoDigitsToNumber . takeDigitsFromLine


workOnContents :: String -> Int
workOnContents = sum . map workOnLine . lines

simpleTask :: String -> [[Int]]
simpleTask content = map takeDigitsFromLine $ lines content