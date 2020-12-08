module Lib
    ( someFunc
    ) where

import Text.Regex.Base
import Text.Regex.PCRE((=~))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- -1 to take off "shiny gold" itself:
-- someFunc = (+) (-1) . Set.size . bagsEventuallyContaining (Set.singleton "shiny gold") . Map.fromList . map parseLine

-- part two
someFunc = numberOfBagsInside "shiny gold" . Map.fromList . map parseLine

parseLine :: String -> (String, [(Int, String)])
parseLine s
  -- basically assume this won't happen:
  | length thisBag == 0 = ("", [])
  | otherwise = (thisBag!!0!!1, map parseBag containsBags)
  where
    thisBag = s =~ "^(.*) bags contains?" :: [[String]]
    containsBags = s =~ "(\\d+) ([^,]+) bag" :: [[String]]

parseBag :: [String] -> (Int, String)
parseBag x = (read $ x!!1, x!!2)

bagsEventuallyContaining :: Set.Set String -> Map.Map String [(Int, String)] -> Set.Set String
bagsEventuallyContaining i m
  | length o == length i = i
  | otherwise = bagsEventuallyContaining o m
  where o = bagsDirectlyContaining i m

bagsDirectlyContaining :: Set.Set String -> Map.Map String [(Int, String)] -> Set.Set String
bagsDirectlyContaining s m = Set.union s . Set.fromList . map (\(v, deps) -> v) . filter (\(v, deps) -> any (\(n, dep) -> Set.member dep s) deps) $ Map.toList m

numberOfBagsInside :: String -> Map.Map String [(Int, String)] -> Int
numberOfBagsInside b m = sum . map (\(x, innerb) -> (+) x . (*) x $ numberOfBagsInside innerb m) $ m Map.! b