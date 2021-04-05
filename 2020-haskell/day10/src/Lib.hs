module Lib
    ( someFunc
    ) where

import Data.List as List
import qualified Data.Map.Strict as Map

-- part I
-- someFunc :: [String] -> Int
-- someFunc = (\m -> (m Map.! 1) * (m Map.! 3)) . counts (Map.fromList [(1, 0), (2, 0), (3, 0)]) . differences . (\xs -> (0:xs) ++ [(+) 3 $ last xs]) . List.sort . map readInt
someFunc :: [String] -> Int
someFunc = differentPermutations . (\xs -> (0:xs) ++ [(+) 3 $ last xs]) . List.sort . map readInt

readInt :: String -> Int
readInt = read

differences :: [Int] -> [Int]
differences l = zipWith (-) (drop 1 l) l

counts :: Map.Map Int Int -> [Int] -> Map.Map Int Int
counts m [] = m
counts m (x:xs) = counts (Map.adjust (+1) x m) xs

-- part II
-- from (x, n) add n to any x <= x+3, start with (0, 1)
differentPermutations :: [Int] -> Int
differentPermutations = countAsYouGoAlong . map (\x -> if x == 0 then (0, 1) else (x, 0))

countAsYouGoAlong :: [(Int, Int)] -> Int
-- for completeness, shouldn't happen:
countAsYouGoAlong [] = 0
-- final number:
countAsYouGoAlong [(x, n)] = n
countAsYouGoAlong ((x, n):xs) = countAsYouGoAlong . map (\(x', n') -> if x' - x <= 3 then (x', n' + n) else (x', n')) $ xs
