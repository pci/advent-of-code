module Lib
  ( someFunc,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Regex.Base
import Text.Regex.PCRE ((=~))
import Utils (join, split)

data Food = Food {ingredients :: [String], allegens :: [String]}
  deriving (Eq, Show)

someFunc :: [String] -> String
someFunc = partII . parseInput

partI :: [Food] -> Int
partI foods = length . filter (`S.notMember` allOptions) . concatMap ingredients $ foods
  where
    possibles = toPotential M.empty foods
    allOptions = toBigSet possibles

partII :: [Food] -> String
partII = partIIOutput . (\m -> iterSolver (m, M.empty)) . toPotential M.empty

parseInput :: [String] -> [Food]
parseInput = map parseLine

parseLine :: String -> Food
parseLine s = Food {ingredients = filter (not . null) . split ' ' $ r !! 1, allegens = split ' ' . filter (/= ',') $ r !! 3}
  where
    res = s =~ "(([a-z]+ )+)\\(contains (([a-z]+(, )?)+)" :: [[String]]
    r = head res

toPotential :: M.Map String (S.Set String) -> [Food] -> M.Map String (S.Set String)
toPotential m [] = m
toPotential m (x : xs) = (`toPotential` xs) $ foldr (M.alter (setIntersection . S.fromList . ingredients $ x)) m (allegens x)

setIntersection :: Ord a => S.Set a -> Maybe (S.Set a) -> Maybe (S.Set a)
setIntersection x (Just y) = Just $ S.intersection x y
setIntersection x Nothing = Just x

toBigSet :: M.Map String (S.Set String) -> S.Set String
toBigSet = foldr S.union S.empty

iterSolver :: (M.Map String (S.Set String), M.Map String String) -> M.Map String String
iterSolver (allegensToPossibleIngredients, found)
  | M.size allegensToPossibleIngredients == 0 = found
  | otherwise = iterSolver . M.foldrWithKey (\allegen ingred (currA2P, currfound) -> (updatePossibilities allegen ingred currA2P, M.insert allegen ingred currfound)) (allegensToPossibleIngredients, found) . M.map (head . S.toList) . M.filter ((==) 1 . S.size) $ allegensToPossibleIngredients

-- remove allegen and ingredient from other options:
updatePossibilities :: String -> String -> M.Map String (S.Set String) -> M.Map String (S.Set String)
updatePossibilities allegen ingred = M.map (S.delete ingred) . M.delete allegen

partIIOutput :: M.Map String String -> String
partIIOutput = join "," . map snd . M.toAscList