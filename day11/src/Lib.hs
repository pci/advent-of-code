module Lib
    ( someFunc
    ) where

import Data.List as List

-- part I
-- someFunc :: [String] -> Int
-- someFunc = countPeople . runUntilSteadyState . map readLine
-- part II
someFunc :: [String] -> Int
someFunc = countPeople . runUntilSteadyState . map readLine

data Space = Floor | Chair | Person
  deriving (Eq, Show)

readLine :: String -> [Space]
readLine = map readChar

readChar :: Char -> Space
readChar '.' = Floor
readChar 'L' = Chair
readChar '#' = Person

runUntilSteadyState :: [[Space]] -> [[Space]]
runUntilSteadyState s
  | difference s next == 0 = s
  | otherwise = runUntilSteadyState next
  where next = stepTime s

stepTime :: [[Space]] -> [[Space]]
stepTime s = map (map (\(p, a) -> stepSpace p a)) $ zipWith (zip) s adj
    where adj = genLineOfSightAdjacency s

stepSpace :: Space -> Int -> Space
stepSpace Floor _ = Floor
stepSpace Chair 0 = Person
stepSpace s a
  -- you have to change this back to 4 for part I
  | s == Person && a >= 5 = Chair
  | otherwise = s

-- how many occupied seats am I next to?
genAdjacency :: [[Space]] -> [[Int]]
-- shift left, up, right, down, and diagonals then sum
genAdjacency i = addAll [nw, n, ne, e, se, s, sw, w]
  where
    g = personsToOne i
    n = moveUp g
    e = moveRight g
    s = moveDown g
    w = moveLeft g
    nw = moveLeft n
    ne = moveRight n
    sw = moveLeft s
    se = moveRight s

addAll :: Num a => [[[a]]] -> [[a]]
addAll [x] = x
addAll (x:xs) = addArray x $ addAll xs

addArray :: Num a => [[a]] -> [[a]] -> [[a]]
addArray = zipWith (zipWith (+))

personsToOne :: [[Space]] -> [[Int]]
personsToOne = map (map (\x -> if x == Person then 1 else 0))

moveDown :: [[Int]] -> [[Int]]
moveDown x = init ((map (\_ -> 0) $ head x):x)

moveUp :: [[Int]] -> [[Int]]
moveUp x = (drop 1 x) ++ [map (\_ -> 0) $ head x]

moveLeft :: [[Int]] -> [[Int]]
moveLeft = map (\l -> (drop 1 l) ++ [0])

moveRight :: [[Int]] -> [[Int]]
moveRight = map (\l -> (0:(init l)))

difference :: [[Space]] -> [[Space]] -> Int
difference a b = sum $ zipWith diffLine a b

diffLine :: [Space] -> [Space] -> Int
diffLine a b = sum $ zipWith (\x -> (\y -> if x == y then 0 else 1)) a b

countPeople :: [[Space]] -> Int
countPeople = sum . concat . personsToOne

-- part II
genLineOfSightAdjacency :: [[Space]] -> [[Int]]
genLineOfSightAdjacency s = map (\(l, y) -> map (\(v, x) -> calcLineOfSightSum s (x, y)) $ zip l [0..]) $ zip s [0..]

calcLineOfSightSum :: [[Space]] -> (Int, Int) -> Int
-- the (x+dx, y+dy) is to nudge it off itself
calcLineOfSightSum s (x, y) = sum $ map (\(dx, dy) -> calcLineOfSight s (x+dx, y+dy) (dx, dy)) [(-1,1), (0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0)]

calcLineOfSight :: [[Space]] -> (Int, Int) -> (Int, Int) -> Int
calcLineOfSight s (x, y) (dx, dy)
  | x < 0 = 0
  | y < 0 = 0
  | x >= (length (s!!0)) = 0
  | y >= (length s) = 0
  | s!!y!!x == Chair = 0
  | s!!y!!x == Person = 1
  | otherwise = calcLineOfSight s (x+dx, y+dy) (dx, dy)