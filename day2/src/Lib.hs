module Lib
    ( someFunc
    ) where

import Text.Regex.Base
import Text.Regex.PCRE((=~)) -- or DFA or PCRE or PosixRE

someFunc :: [String] -> Int
someFunc = length . filter (checkValidity2 . processLine)
-- someFunc ss = (\l -> (((s l)!!(x l - 1) == (c l)) /= ((s l)!!(y l - 1) == (c l)))) . processLine $ "5-6 m: mmbmmlvmbmmgmmf"

data RuleAndLine = RuleAndLine {
   x :: Int
,  y :: Int
,  c        :: Char
,  s        :: String
}
  deriving (Eq, Show)

processLine :: String -> RuleAndLine
processLine s = do
  let res = s =~ "(\\d+)-(\\d+) ([a-z]): ([a-z]+)" :: [[String]]
  RuleAndLine {x = (read $ res!!0!!1), y = (read $ res!!0!!2), c = (res!!0!!3!!0), s = (res!!0!!4)}

checkValidity1 :: RuleAndLine -> Bool
checkValidity1 l = do
  let n = length $ filter (== (c l)) (s l)
  n >= (x l) && n <= (y l)

checkValidity2 :: RuleAndLine -> Bool
-- check the length and one and only oneof the positions matches:
checkValidity2 l = ((length (s l)) >= (y l)) && (((s l)!!(x l - 1) == (c l)) /= ((s l)!!(y l - 1) == (c l)))