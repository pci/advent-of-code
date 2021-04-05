module Lib
  ( someFunc,
  )
where

import GHC.Base (divInt)

someFunc :: [String] -> Int
someFunc = powMod n 7 . product . map (findPowerModN . read)

n :: Int
n = 20201227

-- the RSA numbers in this question are so
-- low, I'm just going to brute force a few calcs:
findPowerModN :: Int -> Int
findPowerModN search = findPowerModInner 7 7 search n

findPowerModInner :: Int -> Int -> Int -> Int -> Int
findPowerModInner curr x search m
  | curr == search = 1
  | otherwise = 1 + findPowerModInner next x search m
  where
    next = (curr * x) `mod` m

-- x^e mod m
powMod :: Int -> Int -> Int -> Int
powMod m x e
  | e == 1 = x `mod` m
  | e == 2 = (x * x) `mod` m
  | even e = powMod m ((x * x) `mod` m) (divInt e 2)
  | otherwise = (`mod` m) $ x * powMod m ((x * x) `mod` m) (divInt (e -1) 2)
