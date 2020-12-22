module Lib
  ( someFunc,
  )
where

import qualified Data.Set as S
import Utils (split)

-- part I
-- part II could make it difficult in a bunch of
-- ways (more players, more rules, loops, etc.)
-- so don't overthink part one
-- someFunc :: [String] -> Int
-- someFunc = calcScorePartI . uncurry playPartI . parseInput

-- part II
someFunc :: [String] -> Int
someFunc = calcScorePartI . snd . uncurry (playPartII S.empty) . parseInput

parseInput :: [String] -> ([Int], [Int])
parseInput = (\l -> (head l, l !! 1)) . map (map read . tail) . split ""

-- assume no loops
playPartI :: [Int] -> [Int] -> [Int]
playPartI [] x = x
playPartI x [] = x
playPartI (x : xs) (y : ys)
  | x > y = playPartI (xs ++ [x, y]) ys
  | otherwise = playPartI xs (ys ++ [y, x])

calcScorePartI :: [Int] -> Int
calcScorePartI = sum . zipWith (*) [1 ..] . reverse

-- part II

playPartII :: S.Set ([Int], [Int]) -> [Int] -> [Int] -> (Int, [Int])
playPartII _ [] x = (2, x)
playPartII _ x [] = (1, x)
playPartII s (x : xs) (y : ys)
  | S.member (x : xs, y : ys) s = (1, x : xs)
  | x > y && (x > length xs || y > length ys) = playPartII nextSet (xs ++ [x, y]) ys
  | y > x && (x > length xs || y > length ys) = playPartII nextSet xs (ys ++ [y, x])
  | otherwise = if fst subGame == 1 then playPartII nextSet (xs ++ [x, y]) ys else playPartII nextSet xs (ys ++ [y, x])
  where
    nextSet = S.insert (x : xs, y : ys) s
    -- laziness for the win:
    subGame = playPartII S.empty (take x xs) (take y ys)
