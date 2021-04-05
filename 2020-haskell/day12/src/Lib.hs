module Lib
    ( someFunc
    ) where

-- part I
-- someFunc :: [String] -> Int
-- someFunc = manhattenDistance start . applyWaypoint start . map readInstruction
-- part II
someFunc :: [String] -> Int
someFunc = manhattenDistance startWaypoint . applyWaypoint startWaypoint . map readInstruction

data Instruction = F Int | L Int | R Int | N Int | S Int | W Int | E Int
  deriving (Eq, Show)

data Position = Position { 
    n :: Int, e :: Int, dir :: (Int, Int)
}
  deriving (Eq, Show)

start :: Position
start = Position{n=0,e=0,dir=(1,0)}

readInstruction :: String -> Instruction
readInstruction s = readInstructionInner (head s) (tail s)

readInstructionInner :: Char -> String -> Instruction
readInstructionInner 'F' s = F (read s)
readInstructionInner 'L' s = L (read s)
readInstructionInner 'R' s = R (read s)
readInstructionInner 'N' s = N (read s)
readInstructionInner 'S' s = S (read s)
readInstructionInner 'E' s = E (read s)
readInstructionInner 'W' s = W (read s)

apply :: Position -> [Instruction] -> Position
apply p [] = p
apply p (x:xs) = apply (applyInstruction p x) xs

applyInstruction :: Position -> Instruction -> Position
-- apply the current heading x times
applyInstruction p (F x) = p{n = (n p) + x*(snd $ dir p), e = (e p) + x*(fst $ dir p)}
applyInstruction p (N x) = p{n = (n p) + x}
applyInstruction p (S x) = p{n = (n p) - x}
applyInstruction p (E x) = p{e = (e p) + x}
applyInstruction p (W x) = p{e = (e p) - x}
-- assume 90 degree multiple
applyInstruction p (L 0) = p
applyInstruction p (R 0) = p
applyInstruction p (L x) = applyInstruction p{dir = (-(snd $ dir p), fst $ dir p)} (L (x-90)) 
applyInstruction p (R x) = applyInstruction p{dir = (snd $ dir p, -(fst $ dir p))} (R (x-90)) 

manhattenDistance :: Position -> Position -> Int
manhattenDistance a b = abs(n a - n b) + abs(e a - e b)

-- part II
startWaypoint :: Position
startWaypoint = Position{n=0,e=0,dir=(10,1)}

applyWaypoint :: Position -> [Instruction] -> Position
applyWaypoint p [] = p
applyWaypoint p (x:xs) = applyWaypoint (applyWaypointInstruction p x) xs

applyWaypointInstruction :: Position -> Instruction -> Position
-- apply the current heading x times
applyWaypointInstruction p (F x) = p{n = (n p) + x*(snd $ dir p), e = (e p) + x*(fst $ dir p)}
applyWaypointInstruction p (N x) = p{dir = ((fst $ dir p), (snd $ dir p) + x)}
applyWaypointInstruction p (S x) = p{dir = ((fst $ dir p), (snd $ dir p) - x)}
applyWaypointInstruction p (E x) = p{dir = ((fst $ dir p) + x, (snd $ dir p))}
applyWaypointInstruction p (W x) = p{dir = ((fst $ dir p) - x, (snd $ dir p))}
-- assume 90 degree multiple
applyWaypointInstruction p (L 0) = p
applyWaypointInstruction p (R 0) = p
applyWaypointInstruction p (L x) = applyInstruction p{dir = (-(snd $ dir p), fst $ dir p)} (L (x-90)) 
applyWaypointInstruction p (R x) = applyInstruction p{dir = (snd $ dir p, -(fst $ dir p))} (R (x-90)) 