module Lib
  ( someFunc,
  )
where

import qualified Data.Map as M

-- part I
-- someFunc :: String -> String
-- someFunc = partIOutput . (!! 100) . iterate partIStep . parseInput

parseInput :: String -> [Int]
parseInput = map (read . (: []))

partIStep :: [Int] -> [Int]
partIStep (curr : x : y : z : res) = take n . dropWhile (/= head res) . cycle . take n . (\nextres -> dest : x : y : z : nextres) . drop 1 . dropWhile (/= dest) . cycle . (curr :) $ res
  where
    n = length res + 4
    dest = findDest n (curr -1) [x, y, z]

findDest :: Int -> Int -> [Int] -> Int
findDest n curr exclude
  | curr == 0 = findDest n n exclude
  | curr `elem` exclude = findDest n (curr -1) exclude
  | otherwise = curr

partIOutput :: [Int] -> String
partIOutput x = concatMap show . drop 1 . take n . dropWhile (/= 1) . cycle $ x
  where
    n = length x

-- part II
-- sadly I think I need to do a memory efficient
-- algorithm as this can't even to 100 iterations
-- so is copying memory at some point, will switch to
-- a lower level langauge, work continues in day23.go
someFunc :: String -> (Int, Int)
someFunc = partIIOutput . (!! 100) . iterate partIStep . partIIGenerateLongList . parseInput

partIIGenerateLongList :: [Int] -> [Int]
partIIGenerateLongList x = x ++ [(n + 1) .. 1000000]
  where
    n = length x

partIIOutput :: [Int] -> (Int, Int)
partIIOutput = (\x -> (x !! 1, x !! 2)) . dropWhile (/= 1) . cycle

cycleDetection :: Int -> M.Map [Int] Int -> [Int] -> (Int, Int, [Int])
cycleDetection itr m input
  | M.member input m = (m M.! input, itr, input)
  | otherwise = cycleDetection (itr + 1) (M.insert input itr m) (partIStep input)