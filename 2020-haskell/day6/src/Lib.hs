module Lib
    ( someFunc
    ) where

import Utils (split, join)
import Data.Set (Set, fromList, size, intersection, empty)

-- someFunc :: [String] -> Int
someFunc = sum . map size . map processGroup2 . split ""

processGroup1 :: [String] -> Set Char
-- doesn't actually matter who says what, so join into one big line
processGroup1 = fromList . join ""


processGroup2 :: [String] -> Set Char
-- need to know the questions *everyone* in the group answered yes to - intersection of the sets
processGroup2 = intersections . map fromList

-- For some reason Data.Set doesn't do one of these, we'll just do it
-- for lists, but easy enough to expand to Foldables
intersections :: Ord a => [Set a] -> Set a
intersections [] = empty
intersections [x] = x
intersections (x:xs) = intersection x $ intersections xs
