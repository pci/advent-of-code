module Main where

import Lib

main = do
    contents <- getContents
    putStrLn . show . someFunc . lines $ contents
