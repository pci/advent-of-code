module Lib
  ( someFunc,
  )
where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Text.Regex.Base
import Text.Regex.PCRE ((=~))
import Utils (split)

-- Part I
-- someFunc :: [String] -> Int
-- someFunc = (\(rules, _, others) -> partOne rules (concat others) 0) . parseInput . split ""

data Rule = Rule {name :: String, lower1 :: Int, upper1 :: Int, lower2 :: Int, upper2 :: Int}
  deriving (Eq, Ord, Show)

checkRule :: Rule -> Int -> Bool
checkRule r i = ((i >= lower1 r) && (i <= upper1 r)) || ((i >= lower2 r) && (i <= upper2 r))

parseInput :: [[String]] -> ([Rule], [Int], [[Int]])
parseInput s
  | length s /= 3 = ([], [], [])
  | otherwise = (map parseRule . head $ s, map read . split ',' $ s !! 1 !! 1, map (map read . split ',') . tail $ s !! 2)

parseRule :: String -> Rule
parseRule s = Rule {name = head res !! 1, lower1 = read $ head res !! 2, upper1 = read $ head res !! 3, lower2 = read $ head res !! 4, upper2 = read $ head res !! 5}
  where
    res = s =~ "([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)" :: [[String]]

-- I don't know which way part II will go, so just going to go
-- simple with part one:
partOne :: [Rule] -> [Int] -> Int -> Int
partOne _ [] i = i
partOne rules (x : xs) i
  | valid = partOne rules xs i
  | not valid = partOne rules xs (i + x)
  where
    valid = any (`checkRule` x) rules

-- Part II
someFunc :: [String] -> Maybe Int
someFunc s = do
  (rules, ticket, others) <- return . filterInvalid . parseInput . split "" $ s
  ordering <- findResult (rules, ticket, others)
  return . calculateFinalValue . yourTicket ticket $ ordering

filterInvalid :: ([Rule], [Int], [[Int]]) -> ([Rule], [Int], [[Int]])
filterInvalid (rules, ticket, others) = (rules, ticket, filter (isValid rules) others)

isValid :: [Rule] -> [Int] -> Bool
isValid rules = not . foldr (\x -> (||) (all (\r -> not . checkRule r $ x) rules)) False

findResult :: ([Rule], [Int], [[Int]]) -> Maybe [Rule]
findResult (rules, ticket, others) = findMatch Set.empty . buildOptions others $ rules

buildOptions :: [[Int]] -> [Rule] -> [[Rule]]
buildOptions others rules = map (`potentialRulesForThisList` rules) trans
  where
    trans = List.transpose others

potentialRulesForThisList :: [Int] -> [Rule] -> [Rule]
potentialRulesForThisList testList = filter (\r -> all (checkRule r) testList)

findMatch :: Set.Set Rule -> [[Rule]] -> Maybe [Rule]
findMatch used [lastRules]
  | null choices = Nothing
  | otherwise = Just [head choices]
  where
    choices = filter (`notElem` used) lastRules
findMatch used (candidates : xs) = firstJust (\c -> fmap (c :) . findMatch (Set.insert c used) $ xs) . filter (`notElem` used) $ candidates

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f [] = Nothing
firstJust f (x : xs)
  | Maybe.isJust res = res
  | otherwise = firstJust f xs
  where
    res = f x

yourTicket :: [Int] -> [Rule] -> [(String, Int)]
yourTicket values rules = zip (map name rules) values

calculateFinalValue :: [(String, Int)] -> Int
calculateFinalValue = product . map snd . filter (\(name, v) -> List.isPrefixOf "departure " name)