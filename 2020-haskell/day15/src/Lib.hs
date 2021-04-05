module Lib
    ( someFunc
    ) where

import Utils(split)
import qualified Data.Map as Map

-- part I
-- -1 as we use 0 based indexing in this house!
someFunc :: String -> Int
-- someFunc = runUntil (2020 - 1) . buildInitial . map read . split ','
-- part II
-- there is probably an elegant way, but this only takes 2 minutes to run so...
someFunc = runUntil (30000000 - 1) . buildInitial . map read . split ','

buildInitial :: Ord a => [a] -> (a, Int, Map.Map a Int)
buildInitial l = (last l, length l - 1, buildInitialInner (init l) Map.empty 0)

buildInitialInner :: Ord a => [a] -> Map.Map a Int -> Int -> Map.Map a Int
buildInitialInner [] m _ = m
buildInitialInner (x:xs) m i = buildInitialInner xs (Map.insert x i m) (i+1)

runUntil :: Int -> (Int, Int, Map.Map Int Int) -> Int
runUntil idx (curr, currIdx, m)
  | currIdx == idx = curr
  | otherwise = runUntil idx $ next (curr, currIdx, m)

next :: (Int, Int, Map.Map Int Int) -> (Int, Int, Map.Map Int Int)
next (curr, idx, map)
  | seenBefore = (idx - (map Map.! curr), idx+1, Map.insert curr idx map)
  | otherwise = (0, idx+1, Map.insert curr idx map)
  where seenBefore = Map.member curr map