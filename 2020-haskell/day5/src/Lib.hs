module Lib
    ( someFunc
    ) where

someFunc = findMissing . map processSeat

processSeat :: [Char] -> Int
-- so we can use tail recursion we reverse the order so we get LSB first:
processSeat = processTail . reverse

processTail :: [Char] -> Int
processTail "" = 0
processTail (x:xs)
    | x == 'B' || x == 'R' = (+) 1 . (*) 2 . processTail $ xs
    | x == 'F' || x == 'L' = (*) 2 . processTail $ xs
    | otherwise = processTail xs

findMissing :: [Int] -> Int
findMissing l = maximum $ filter (\x -> notElem x l) [1..(maximum l)]
