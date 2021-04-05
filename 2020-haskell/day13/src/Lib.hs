module Lib
    ( someFunc
    ) where

import Utils (split, join)
import Data.List (minimumBy, maximumBy)

-- Part I
-- someFunc :: [String] -> Int
-- someFunc s = (\(b,w) -> b*w) . findMin . calculateWaits (read $ s!!0) . readInput $ s!!1

readInput :: String -> [Int]
readInput = map read . filter (/= "x") . split ','

calculateWaits :: Int -> [Int] -> [(Int, Int)]
calculateWaits s = map (\b -> (b, (b - s) `mod` b))

findMin :: [(Int, Int)] -> (Int, Int)
findMin = minimumBy compBySnd

compByFst :: Ord a => (a, b) -> (a, b) -> Ordering
compByFst x y = compare (fst x) (fst y)

compBySnd :: Ord b => (a, b) -> (a, b) -> Ordering
compBySnd x y = compare (snd x) (snd y)

-- part II
-- I'm sure there's a very elegant modulo arithmatic solution
-- here, however I've only had one coffee..
-- Brute force off mulitple jumps of the largest number take too long
-- All offsets being zero would just ba a nice lcm(inputs...), but the offsets
-- make it tricky, going to use tail recurssion - find a solution for the first
-- n elements, then step in steps of lcm(i_0, ..., i_(n-1)) to find the solution
-- to the first (n+1) elements, then repeat:

someFunc :: [String] -> Int
someFunc s = findFirstForAll (0, 1) . readInput' $ s!!1

readInput' :: String -> [(Int, Int)]
readInput' = map (\(o, b) -> (read b, o)) . filter (\(o, b) -> b /= "x") . zip [0..] . split ','

findFirstForAll :: (Int, Int) -> [(Int, Int)] -> Int
findFirstForAll (start, step) [] = start
findFirstForAll (start, step) (x:xs) = findFirstForAll (findNext (start, step) x) xs


findNext :: (Int, Int) -> (Int, Int) -> (Int, Int)
findNext (start, step) (bus, offset)
  -- once have _a_ solution, this will repeat evey lcm(step, bus):
  | (start + offset) `mod` bus == 0 = (start, lcm step bus) 
  | otherwise = findNext (start+step, step) (bus, offset)