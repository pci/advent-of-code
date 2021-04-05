module Main where

import Lib

main = do
  contents <- getContents
  putStrLn . show . fmap multiplyTriplet . findSum 2020 . map itoa . lines $ contents

itoa :: String -> Int
itoa = read

findSum :: Int -> [Int] -> Maybe (Int, (Int, Int))
findSum n l = maybeFirst . filter (\(a,(b, c)) -> a + b + c == n) . (createGroups l) $ createGroups l l

createGroups :: [a] -> [b] -> [(a, b)]
createGroups [] x = []
createGroups (x:xs) y = map ((,) x) y ++ createGroups xs y

maybeFirst :: [a] -> Maybe a
maybeFirst [] = Nothing
maybeFirst (x:y) = Just x

multiplyPair :: (Int, Int) -> Int
multiplyPair (a, b) = a * b

multiplyTriplet :: (Int, (Int, Int)) -> Int
multiplyTriplet (a, (b, c)) = a * b * c
