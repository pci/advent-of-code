module Lib
  ( someFunc,
  )
where

import Control.Monad (guard)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Pos3D = Pos3D {x :: Int, y :: Int, z :: Int}
  deriving (Eq, Show, Ord)

class Position p where
  neighbours :: [p]
  add :: p -> p -> p

instance Position Pos3D where
  neighbours = neighboursDeltas3D
  add = \p q -> Pos3D {x = x p + x q, y = y p + y q, z = z p + z q}

data Pos4D = Pos4D {x4d :: Int, y4d :: Int, z4d :: Int, w4d :: Int}
  deriving (Eq, Show, Ord)

instance Position Pos4D where
  neighbours = neighboursDeltas4D
  add = \p q -> Pos4D {x4d = x4d p + x4d q, y4d = y4d p + y4d q, z4d = z4d p + z4d q, w4d = w4d p + w4d q}

-- Part I
-- You don't _actual_ have to consider z < 0 as
-- it'll mirror z > 0, but it's just easier to and at most
-- doubles run time
someFunc :: [String] -> Int
-- someFunc = Set.size . nextN 6 . processInitialInput3D
-- Part II
-- ...ok, how cool are typeclasses!
someFunc = Set.size . nextN 6 . processInitialInput4D

processInitialInput3D :: [String] -> Set.Set Pos3D
processInitialInput3D = Set.fromList . map (\(x, y, _) -> Pos3D {x = x, y = y, z = 0}) . filter (\(_, _, v) -> v == '#') . concatMap (\(y, row) -> zipWith (\x v -> (x, y, v)) [0 ..] row) . zip [0 ..]

processInitialInput4D :: [String] -> Set.Set Pos4D
processInitialInput4D = Set.fromList . map (\(x, y, _) -> Pos4D {x4d = x, y4d = y, z4d = 0, w4d = 0}) . filter (\(_, _, v) -> v == '#') . concatMap (\(y, row) -> zipWith (\x v -> (x, y, v)) [0 ..] row) . zip [0 ..]

nextN :: (Ord k, Position k) => Int -> Set.Set k -> Set.Set k
nextN 0 p = p
nextN i p = nextN (i -1) (next p)

next :: (Ord k, Position k) => Set.Set k -> Set.Set k
next prev = neighboursToNextGeneration prev $ generateNeighbours prev

generateNeighbours :: (Foldable a, Ord k, Position k) => a k -> Map.Map k Int
generateNeighbours positions = foldr generateNeighboursIter (generateEmpties positions) positions

generateEmpties :: (Foldable a, Ord k) => a k -> Map.Map k Int
generateEmpties = foldr (`Map.insert` 0) Map.empty

generateNeighboursIter :: (Ord pos, Position pos) => pos -> Map.Map pos Int -> Map.Map pos Int
generateNeighboursIter p m =
  foldr
    ( (\p' m -> Map.insertWith (+) p' 1 m)
        . add p
    )
    m
    neighbours

-- Referential transparency FTW
neighboursDeltas3D :: [Pos3D]
neighboursDeltas3D = do
  x <- [-1 .. 1]
  y <- [-1 .. 1]
  z <- [-1 .. 1]
  -- look a guard! I think I even used it properly this time!
  guard (x /= 0 || y /= 0 || z /= 0)
  return Pos3D {x = x, y = y, z = z}

neighboursDeltas4D :: [Pos4D]
neighboursDeltas4D = do
  x <- [-1 .. 1]
  y <- [-1 .. 1]
  z <- [-1 .. 1]
  w <- [-1 .. 1]
  -- look a guard! I think I even used it properly this time!
  guard (x /= 0 || y /= 0 || z /= 0 || w /= 0)
  return Pos4D {x4d = x, y4d = y, z4d = z, w4d = w}

neighboursToNextGeneration :: Ord k => Set.Set k -> Map.Map k Int -> Set.Set k
neighboursToNextGeneration = Map.foldrWithKey insertOrDelete

insertOrDelete :: Ord k => k -> Int -> Set.Set k -> Set.Set k
insertOrDelete p n actives
  | Set.member p actives && (n < 2 || n > 3) = Set.delete p actives
  | (not . Set.member p $ actives) && n == 3 = Set.insert p actives
  | otherwise = actives

printCurrent3D :: Set.Set Pos3D -> String
printCurrent3D s = unlines $ concatMap (\z -> ("z=" ++ show z) : map (\y -> map (\x -> if Set.member Pos3D {x = x, y = y, z = z} s then '#' else '.') [(- maxX) .. maxX]) [(- maxY) .. maxY]) [(- maxZ) .. maxZ]
  where
    maxX = maximum . map (abs . x) . Set.toList $ s
    maxY = maximum . map (abs . y) . Set.toList $ s
    maxZ = maximum . map (abs . z) . Set.toList $ s