module Lib
    ( someFunc
    ) where

import Text.Regex.Base
import Text.Regex.PCRE((=~))
import qualified Data.Set as Set

data Instruction = Noop Int | Acc Int | RelativeJump Int
  deriving (Eq, Show)

data ExitCode = ExitNormal | ExitOvershoot | ExitInfiniteLoop
  deriving (Eq, Show)

data State = State {
    at :: Int, acc :: Int, visited :: Set.Set Int, exit :: ExitCode
}
  deriving (Eq, Show)

-- Part I
-- someFunc :: [String] -> State
-- someFunc = checkAndRun State {at = 0, acc = 0, visited = Set.empty, exit = ExitNormal} . map parseLine
-- Part II
someFunc :: [String] -> [State]
someFunc = filter (\s -> (exit s) == ExitNormal) . map (checkAndRun State {at = 0, acc = 0, visited = Set.empty, exit = ExitNormal}) . generateAllTheCreazyInstructionSets . map parseLine

parseLine :: String -> Instruction
parseLine s
  | length res == 0 = Noop 0
  | otherwise = generateInstruction (res!!0!!1, read $ res!!0!!2)
  where res = s =~ "(nop|acc|jmp) \\+?(\\-?\\d+)" :: [[String]]

generateInstruction :: (String, Int) -> Instruction
generateInstruction ("nop", x) = Noop x
generateInstruction ("acc", x) = Acc x
generateInstruction ("jmp", x) = RelativeJump x

checkAndRun :: State -> [Instruction] -> State
checkAndRun s i
  | (at s) > length i = s {exit = ExitOvershoot}
  | (at s) == length i = s {exit = ExitNormal}
  | Set.member (at s) (visited s) = s {exit = ExitInfiniteLoop}
  | otherwise = checkAndRun (run ((!!) i $ at s) s) i

run :: Instruction -> State -> State
run (Noop x) s = s {at = (at s) + 1, visited = Set.insert (at s) (visited s)}
run (Acc x) s = s {at = (at s) + 1, acc = (acc s) + x, visited = Set.insert (at s) (visited s)}
run (RelativeJump x) s = s {at = (at s) + x, acc = (acc s), visited = Set.insert (at s) (visited s)}

-- Generate all of the possible changed instruction sets:
generateAllTheCreazyInstructionSets :: [Instruction] -> [[Instruction]]
generateAllTheCreazyInstructionSets l = filter (\x -> length x > 0) . map (\(x, i) -> changeAt i x l) $ zip l [0..]

changeAt :: Int -> Instruction -> [Instruction] -> [Instruction]
changeAt n (Noop x) l = take n l ++ [RelativeJump x] ++ drop (n + 1) l
changeAt n (RelativeJump x) l = take n l ++ [Noop x] ++ drop (n + 1) l
changeAt n (Acc x) l = []