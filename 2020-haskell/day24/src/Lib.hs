module Lib
  ( someFunc,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S

data Direction = NE | E | SE | SW | W | NW
  deriving (Eq, Show)

-- not orthoganal but use (e-w, nw-se) co-ords:
toDelta :: Direction -> (Int, Int)
toDelta E = (1, 0)
toDelta W = (-1, 0)
toDelta NW = (0, 1)
toDelta SE = (0, -1)
toDelta NE = (1, 1)
toDelta SW = (-1, -1)

someFunc :: [String] -> Int
-- someFunc = S.size . partI S.empty . map parseLine
someFunc = S.size . (!! 100) . iterate partIIStepOneDay . partI S.empty . map parseLine

parseLine :: String -> [Direction]
parseLine "" = []
parseLine ('n' : 'e' : res) = NE : parseLine res
parseLine ('n' : 'w' : res) = NW : parseLine res
parseLine ('s' : 'e' : res) = SE : parseLine res
parseLine ('s' : 'w' : res) = SW : parseLine res
parseLine ('e' : res) = E : parseLine res
parseLine ('w' : res) = W : parseLine res

vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (a, b) (c, d) = (a + c, b + d)

-- just keep track of the black tiles:
partI :: S.Set (Int, Int) -> [[Direction]] -> S.Set (Int, Int)
partI s [] = s
partI s (dir : res)
  | S.member delta s = partI (S.delete delta s) res
  | otherwise = partI (S.insert delta s) res
  where
    delta = foldr (vecAdd . toDelta) (0, 0) dir

-- again track black tiles:
partIIStepOneDay :: S.Set (Int, Int) -> S.Set (Int, Int)
partIIStepOneDay s = partIIApplyMap s $ partIIGenerateCountMap s

partIIGenerateCountMap :: S.Set (Int, Int) -> M.Map (Int, Int) Int
partIIGenerateCountMap = foldr (\pos m -> foldr (addToMap . vecAdd pos) m allDir) M.empty

addToMap :: Ord k => k -> M.Map k Int -> M.Map k Int
addToMap = M.alter addOneToMap

addOneToMap :: Maybe Int -> Maybe Int
addOneToMap Nothing = Just 1
addOneToMap (Just x) = Just (x + 1)

partIIApplyMap :: S.Set (Int, Int) -> M.Map (Int, Int) Int -> S.Set (Int, Int)
partIIApplyMap s m =
  S.fromList
    [ pos | pos <- S.toList s ++ M.keys m, (S.member pos s && M.findWithDefault 0 pos m > 0 && M.findWithDefault 0 pos m < 3)
                                             || (S.notMember pos s && M.findWithDefault 0 pos m == 2)
    ]

allDir :: [(Int, Int)]
allDir = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1)]