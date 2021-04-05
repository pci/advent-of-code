module Lib
  ( someFunc,
  )
where

import qualified Data.Map as M
import Text.Regex.Base
import Text.Regex.PCRE ((=~))
import Utils (split)

someFunc :: [String] -> Int
someFunc = length . testStrings . parseInput

data Rule = RuleChar Char | RuleList [Int] | RuleOpts [Int] [Int]
  deriving (Eq, Show)

parseInput :: [String] -> (M.Map Int Rule, [String])
parseInput = (\parts -> (M.fromList . map parseRule . head $ parts, parts !! 1)) . split ""

parseRule :: String -> (Int, Rule)
parseRule s
  | (res !! 0 !! 2 !! 0) == '"' = (read $ res !! 0 !! 1, RuleChar $ res !! 0 !! 2 !! 1)
  | otherwise = (read $ res !! 0 !! 1, parseOptions $ res !! 0 !! 2)
  where
    res = s =~ "(\\d+): (\"[a-z]\"|((\\d+)+ ?(\\| )?)+)" :: [[String]]

parseOptions :: String -> Rule
parseOptions s
  | length (res !! 0 !! 3) > 0 = RuleOpts (map read . filter (/= "") . split ' ' $ res !! 0 !! 1) (map read . split ' ' $ res !! 0 !! 4)
  | otherwise = RuleList . map read . filter (/= "") . split ' ' $ res !! 0 !! 1
  where
    res = s =~ "((\\d+ ?)+)(\\| ((\\d+ ?)+))?" :: [[String]]

testStrings :: (M.Map Int Rule, [String]) -> [String]
testStrings (m, cases) = filter (checkString m) cases

-- very basic implementation:
checkString :: M.Map Int Rule -> String -> Bool
checkString m = (not . null) . filter (== "") . checkRule m (m M.! 0)

checkRule :: M.Map Int Rule -> Rule -> String -> [String]
checkRule _ _ "" = []
checkRule m (RuleChar c) s
  | head s == c = [tail s]
  | otherwise = []
checkRule m (RuleList l) s = foldl (\curr r -> concatMap (checkRule m (m M.! r)) curr) [s] l
checkRule m (RuleOpts opt1 opt2) s = checkRule m (RuleList opt1) s ++ checkRule m (RuleList opt2) s