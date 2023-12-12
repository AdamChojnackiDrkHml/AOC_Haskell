module Main where

import Data.Map (Map)
import qualified Data.Map.Internal.Debug as MD
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
    contents <- readFile "data/data"
    -- putStr . show . simpleTask $ contents
    (print . doWork2) contents

doWork :: String -> Int
doWork = sum
       . map fst
       . filter (\(i, line) -> (gameCheck . T.pack) line)
       . zip [1..]
       . lines

doWork2 :: String -> Int
doWork2 = sum
       . map (gameScore . T.pack)
       . lines

checks :: Map Text Int
checks = M.fromList [(T.pack "red", 12), (T.pack "green", 13), (T.pack "blue", 14)]

colors = map T.pack ["red", "green", "blue"]

insideCheck a (Just b) = a <= b
insideCheck _ Nothing = False

check ::  (T.Text, Int) -> Bool
check (name, a) = insideCheck a (M.lookup name checks)

setCheck :: Text -> Bool
setCheck setLine = allMap check $ setParse setLine

gameCheck :: Text -> Bool
gameCheck = foldl (\acc c -> acc && setCheck c) True . parseGameLine

parseGameLine :: Text -> [Text]
parseGameLine = map T.strip
              . T.splitOn (T.pack ";")
              . T.strip
              . (!! 1)
              . T.splitOn (T.pack ":")

setParse :: T.Text -> Map T.Text Int
setParse = foldl colorParse M.empty
            . map T.strip
            . T.splitOn (T.pack ",")

colorParse :: Map T.Text Int -> Text -> Map T.Text Int
colorParse dict colorSpec =
    let
        [i, color] = take 2 $ T.splitOn (T.pack " ") colorSpec
    in
        M.insert color (read . T.unpack $ i) dict

anyMap :: Ord k => ((k, a) -> Bool) -> Map k a -> Bool
anyMap p m = any p (M.toList m)

allMap :: Ord k => ((k, a) -> Bool) -> Map k a -> Bool
allMap p m = all p (M.toList m)

gameScore :: T.Text -> Int
gameScore gameLine = M.foldl (*) 1 $ parseGame gameLine

parseGame :: T.Text -> Map T.Text Int
parseGame = getMaxes . map setParse . parseGameLine

getMaxes :: [Map T.Text Int] -> Map T.Text Int
getMaxes dicts = M.fromList $ map (\c -> (c, getMaxOfColor dicts c)) colors

appendL :: [a] -> Maybe a -> [a]
appendL l (Just x) = x:l
appendL l Nothing = l

getMaxOfColor :: [Map T.Text Int] -> Text -> Int
getMaxOfColor dicts color =
    maximum $ foldl (\acc c -> appendL acc $ M.lookup color c) [] dicts