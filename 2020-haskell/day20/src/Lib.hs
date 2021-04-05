module Lib
  ( someFunc,
  )
where

import Control.Monad (guard, liftM2)
import Data.List (permutations, transpose)
import qualified Data.Set as S
import GHC.Float (float2Int, int2Float)
import Utils (split)

data Image = Image {imageId :: Int, imageRaw :: [String]}
  deriving (Eq, Ord, Show)

-- Part I
-- someFunc :: [String] -> Int
-- someFunc = calcMultCorners . startFullImage . parseInput

calcMultCorners :: [[Image]] -> Int
calcMultCorners imgs = product . map imageId $ [head . head $ imgs, head . last $ imgs, last . head $ imgs, last . last $ imgs]

-- I'm sure there's a more haskell-y way to do this
-- but buildFullImage =<<'s everything so you get all
-- the solutions (rotations included) concat'd together
startFullImage :: [Image] -> [[Image]]
-- the reverse is due to the efficent head appending
startFullImage imgs = chunksOf n . reverse . head . filter (not . null) . map (uncurry (buildFullImage n)) . concatMap (\i -> map (\r -> (S.delete i fullSet, [r])) (allRotations i)) $ imgs
  where
    fullSet = S.fromList imgs
    n = sqrtInt (length imgs)

buildFullImage :: Int -> S.Set Image -> [Image] -> [Image]
buildFullImage n imgs curr
  | S.size imgs == 0 = curr
  | otherwise = do
    option <- S.toList imgs
    rotation <- allRotations option
    guard $ i < n || imageTop rotation == imageBottom (curr !! (n -1))
    guard $ i `mod` n == 0 || imageLeft rotation == imageRight (head curr)
    buildFullImage n (S.delete option imgs) (rotation : curr)
  where
    i = length curr

-- part II is totally going to use the complete image!
-- so might as well rotate the whole thing:
-- there are 8 different orientations, 4 rotations
-- and their flip. Doesn't matter which.
allRotations :: Image -> [Image]
allRotations i = do
  rotation <- take 4 . iterate rotatePiBy2 $ i
  [rotation, flipHorizontally rotation]

-- rotatePiBy2 rotate clockwise 90 degrees
rotatePiBy2 :: Image -> Image
rotatePiBy2 i =
  i
    { -- 1 2 3    1 4 7    7 4 1
      -- 4 5 6 => 2 5 8 => 8 5 2
      -- 7 8 9    3 6 9    9 6 3
      imageRaw = map reverse . transpose $ imageRaw i
    }

flipHorizontally :: Image -> Image
flipHorizontally i = i {imageRaw = map reverse $ imageRaw i}

parseInput :: [String] -> [Image]
parseInput = map parseTile . split ""

parseTile :: [String] -> Image
parseTile s =
  Image
    { imageId = parseHeader (head s),
      imageRaw = tail s
    }

parseHeader :: String -> Int
parseHeader = read . drop 5 . init

imageTop :: Image -> String
imageTop = head . imageRaw

imageBottom :: Image -> String
imageBottom = last . imageRaw

imageLeft :: Image -> String
imageLeft = map head . imageRaw

imageRight :: Image -> String
imageRight = map last . imageRaw

sqrtInt :: Int -> Int
sqrtInt = float2Int . sqrt . int2Float

chunksOf :: Int -> [a] -> [[a]]
chunksOf n l
  | length l == n = [l]
  | otherwise = take n l : chunksOf n (drop n l)

-- part II
someFunc :: [String] -> Int
someFunc = (\i -> countHashes (imageRaw i) - countHashes nessy * tryTuningThePageAround i) . stitchImage . startFullImage . parseInput

stitchImage :: [[Image]] -> Image
stitchImage = (\d -> Image {imageId = 0, imageRaw = d}) . concatMap ((map concat . transpose) . map (trimImage . imageRaw))

tryTuningThePageAround :: Image -> Int
tryTuningThePageAround = head . filter (/= 0) . map (nessyCount . imageRaw) . allRotations

-- trim one off all sides
trimImage :: [String] -> [String]
trimImage = map (init . tail) . init . tail

-- could manually encode the coords, but a little copy-paste
-- makes this quicker:
nessy :: [String]
nessy =
  [ "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  ]

width :: [[a]] -> Int
width = length . head

height :: [[a]] -> Int
height = length

-- there are better ways, but this isn't going to be run all that much:
lookInTheLoch :: [String] -> Bool
lookInTheLoch s = all ((== '#') . fst) . filter ((== '#') . snd) $ zip (concat s) (concat nessy)

nessyCount :: [String] -> Int
nessyCount img = length . filter lookInTheLoch $ liftM2 (cutout nw nh img) [0 .. (width img - nw)] [0 .. (height img - nh)]
  where
    nw = width nessy
    nh = height nessy

cutout :: Int -> Int -> [String] -> Int -> Int -> [String]
cutout w h source x y = map (take w) . take h . map (drop x) . drop y $ source

countHashes :: [String] -> Int
countHashes = sum . map (length . filter (== '#'))