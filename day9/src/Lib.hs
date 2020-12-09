module Lib
    ( someFunc
    ) where

import Data.Maybe

-- part I
-- someFunc :: [String] -> Int
-- someFunc = findFirstFail . map readInt

-- part II
someFunc :: [String] -> Maybe Int
someFunc s = fmap (\cont -> (minimum cont) + (maximum cont)) $ (\codes -> findContigousMatch codes (findFirstFail codes)) . map readInt $ s

readInt :: String -> Int
readInt = read

findFirstFail :: [Int] -> Int
findFirstFail l = (!!) l $ head . filter (not . hasSum l) $ [n..length l]

hasSum :: [Int] -> Int -> Bool
hasSum l i = hasSumInner (take n . drop (i-n) $ l) $ l!!i

hasSumInner :: [Int] -> Int -> Bool
hasSumInner [] i = False
hasSumInner [x] i = False
hasSumInner (x:xs) i
  | length f > 0 = True
  | otherwise = hasSumInner xs i
  where f = take 1 . filter (\y -> x + y == i) $ xs

findContigousMatch :: [Int] -> Int -> Maybe [Int]
findContigousMatch [] i = Nothing
findContigousMatch [x] i = Nothing
findContigousMatch (x:xs) i = case findContigousMatchInner (x:xs) i of
  Just res -> Just res
  Nothing -> findContigousMatch xs i

findContigousMatchInner :: [Int] -> Int -> Maybe [Int]
-- assume everything's positive
findContigousMatchInner [] aim = Nothing
findContigousMatchInner (x:xs) aim
  | aim < 0 = Nothing
  | x == aim = Just [x]
  | otherwise = fmap (\l -> (x:l)) $ findContigousMatchInner xs (aim - x)

n :: Int
n = 25