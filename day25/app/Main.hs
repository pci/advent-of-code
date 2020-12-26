module Main where

import Lib (someFunc)

main = do
  contents <- getContents
  print . someFunc . lines $ contents
