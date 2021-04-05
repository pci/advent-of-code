module Lib
    ( someFunc
    ) where

import Text.Regex.Base
import Text.Regex.PCRE((=~))
import Data.Bits
import qualified Data.Map as Map

-- Part I 
-- someFunc :: [String] -> Int
-- someFunc = sumMemory . applyInstuctions State { mask = Masks { orMask = 0, andMask = 0, quantumBits = [] }, memory = Map.empty } . map readLine

data Masks = Masks { andMask :: Int, orMask :: Int, quantumBits :: [Int] }
  deriving (Eq, Show)

data Instruction = Mask Masks | SetMem { addr :: Int, rawVal :: Int }
  deriving (Eq, Show)

readLine :: String -> Instruction
readLine s
  | length mask > 0 = generateMask Masks { orMask = 0, andMask = 2^(length $ mask!!0!!1) - 1, quantumBits = [] } $ mask!!0!!1
  | length mem > 0 = SetMem { addr = read $ mem!!0!!1, rawVal = read $ mem!!0!!2 }
  where
    mask = s =~ "mask = ([X10]+)" :: [[String]]
    mem = s =~ "mem\\[(\\d+)\\] = (\\d+)" :: [[String]]

generateMask :: Masks -> String -> Instruction
generateMask m "" = Mask m
generateMask m (x:xs)
  -- in the bits package you talk about bit 0:
  | x == '1' = generateMask m {orMask = setBit (orMask m) (length xs)} xs
  | x == '0' = generateMask m {andMask = clearBit (andMask m) (length xs)} xs
  | otherwise = generateMask m xs

data State = State { mask :: Masks, memory :: Map.Map Int Int }
  deriving (Eq, Show)

applyInstuctions :: State -> [Instruction] -> State
applyInstuctions s [] = s
applyInstuctions s (x:xs) = applyInstuctions (applyInstuction s x) xs

applyInstuction :: State -> Instruction -> State
applyInstuction s (Mask m) = s {mask = m}
applyInstuction s (SetMem m v) = s { memory = Map.insert m (applyMask (mask s) v) (memory s) }

applyMask :: Masks -> Int -> Int
applyMask m = (.&.) (andMask m) . (.|.) (orMask m)

sumMemory :: State -> Int
sumMemory = Map.foldr (+) 0 . memory

-- Part II
someFunc :: [String] -> Int
someFunc = sumMemory . applyInstuctions' State { mask = Masks { orMask = 0, andMask = 0, quantumBits = [] }, memory = Map.empty } . map readLine'

readLine' :: String -> Instruction
readLine' s
  | length mask > 0 = generateMask' Masks { orMask = 0, andMask = 2^(length $ mask!!0!!1) - 1, quantumBits = [] } $ mask!!0!!1
  | length mem > 0 = SetMem { addr = read $ mem!!0!!1, rawVal = read $ mem!!0!!2 }
  where
    mask = s =~ "mask = ([X10]+)" :: [[String]]
    mem = s =~ "mem\\[(\\d+)\\] = (\\d+)" :: [[String]]

generateMask' :: Masks -> String -> Instruction
generateMask' m "" = Mask m
generateMask' m (x:xs)
  -- in the bits package you talk about bit 0:
  | x == '1' = generateMask' m {orMask = setBit (orMask m) (length xs)} xs
  | x == 'X' = generateMask' m {quantumBits = ((length xs):(quantumBits m))} xs
  | otherwise = generateMask' m xs

applyInstuctions' :: State -> [Instruction] -> State
applyInstuctions' s [] = s
applyInstuctions' s (x:xs) = applyInstuctions' (applyInstuction' s x) xs

applyInstuction' :: State -> Instruction -> State
applyInstuction' s (Mask m) = s {mask = m}
applyInstuction' s (SetMem m v) = s { memory = applySetMem' (quantumBits . mask $ s) (m .|. (orMask . mask $ s)) v (memory s) }

applySetMem' :: [Int] -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int
applySetMem' bits baseMem v m = applyMultSet (getAllOptions bits [baseMem]) v m

applyMultSet :: Ord k => [k] -> v -> Map.Map k v -> Map.Map k v
applyMultSet [] _ m = m
applyMultSet (x:xs) v m = applyMultSet xs v (Map.insert x v m)

getAllOptions :: [Int] -> [Int] -> [Int]
getAllOptions [] v = v
getAllOptions (x:xs) values = getAllOptions xs . concat . map (\v -> [setBit v x, clearBit v x]) $ values