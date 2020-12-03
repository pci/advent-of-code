module Lib
    ( someFunc
    ) where

someFunc :: [String] -> Int
someFunc s = product $ map (taboggan $ extendToTheRight s) [(1,1),(3,1),(5,1),(7,1),(1,2)]

-- String is just a [Char] anyway
extendToTheRight :: [String] -> [String]
extendToTheRight = map cycle

taboggan :: [String] -> (Int, Int) -> Int
taboggan = wheeee (0,0)

wheeee :: (Int, Int) -> [String] -> (Int, Int) -> Int
wheeee (x, y) s (dx, dy)
    | y >= length s = 0
    | s!!y!!x == '#' = 1 +  wheeee (x + dx, y  + dy) s (dx, dy)
    | otherwise = wheeee (x + dx, y  + dy) s (dx, dy)