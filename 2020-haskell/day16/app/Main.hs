module Main where

import Lib (someFunc)

main = do
  contents <- getContents
  putStrLn . show . someFunc . lines $ contents
