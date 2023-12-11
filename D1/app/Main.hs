module Main where
import Data.Char (isDigit, digitToInt)
import Data.Text (replace, Text, pack, unpack)
import GHC.IO.FD (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hClose)
import qualified Data.Text as T
main :: IO ()
main = do
    contents <- readFile "D1/data/data"
    -- putStr . show . simpleTask $ contents
    (print . workOnContents) contents


digitsStrings :: [String]
digitsStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "zero"]

digits = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

findAndReplaceLetterDigits :: [(Text, Text)] -> Text ->  Text
findAndReplaceLetterDigits ((x, y):xs) line  = findAndReplaceLetterDigits xs (replace x (T.concat [pack [T.head x], y, pack [T.last x]]) line)
findAndReplaceLetterDigits [] line = line

normalizeLine :: Text -> Text
normalizeLine = findAndReplaceLetterDigits $ mapPair pack $ zip digitsStrings digits

takeDigitsFromLine2 :: String -> [Int]
takeDigitsFromLine2 = firstAndLast. map digitToInt . filter isDigit . unpack . normalizeLine . pack

takeDigitsFromLine :: [Char] -> [Int]
takeDigitsFromLine = firstAndLast. map digitToInt . filter isDigit


firstAndLast :: [a] -> [a]
firstAndLast xs = head xs:[last xs]

twoDigitsToNumber :: Num a => [a] -> a
twoDigitsToNumber (x:y:_) = 10 * x + y
twoDigitsToNumber _ = undefined

workOnLine :: [Char] -> Int
workOnLine = twoDigitsToNumber . takeDigitsFromLine2


workOnContents :: String -> Int
workOnContents = sum . map workOnLine . lines

simpleTask :: String -> [[Int]]
simpleTask content = map takeDigitsFromLine $ lines content

mapPair :: (t -> b) -> [(t, t)] -> [(b, b)]
mapPair f ((a, b):xs) = (f a, f b):mapPair f xs
mapPair f [] = []