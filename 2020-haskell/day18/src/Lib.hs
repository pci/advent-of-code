module Lib
  ( someFunc,
  )
where

-- Part I
-- someFunc :: [String] -> Int
-- someFunc = sum . map (partIRun . splitInLexemes "")

data Op = Add | Mult
  deriving (Show, Eq)

data Lexeme = Value Int | LexOp Op | OpenParen | CloseParen
  deriving (Show, Eq)

splitInLexemes :: String -> String -> [Lexeme]
splitInLexemes acc "" = optParse acc
splitInLexemes _ ('(' : xs) = OpenParen : splitInLexemes "" xs
splitInLexemes _ ('+' : xs) = LexOp Add : splitInLexemes "" xs
splitInLexemes _ ('*' : xs) = LexOp Mult : splitInLexemes "" xs
splitInLexemes acc (')' : xs) = optParse acc ++ CloseParen : splitInLexemes "" xs
splitInLexemes acc (' ' : xs) = optParse acc ++ splitInLexemes "" xs
splitInLexemes acc (x : xs) = splitInLexemes (acc ++ [x]) xs

optParse :: String -> [Lexeme]
optParse "" = []
optParse x = [Value . read $ x]

partIRun :: [Lexeme] -> Int
partIRun = fst . runLTR 0 Add

-- this isn't tidy!
runLTR :: Int -> Op -> [Lexeme] -> (Int, [Lexeme])
runLTR acc _ [] = (acc, [])
runLTR acc _ (CloseParen : xs) = (acc, xs)
runLTR acc _ (LexOp Add : xs) = runLTR acc Add xs
runLTR acc _ (LexOp Mult : xs) = runLTR acc Mult xs
runLTR acc Add (Value x : xs) = runLTR (acc + x) Add xs
runLTR acc Add (OpenParen : xs) = (\(res, left) -> runLTR (acc + res) Add left) $ runLTR 0 Add xs
runLTR acc Mult (Value x : xs) = runLTR (acc * x) Mult xs
runLTR acc Mult (OpenParen : xs) = (\(res, left) -> runLTR (acc * res) Mult left) $ runLTR 0 Add xs

-- Part II
someFunc :: [String] -> Int
someFunc = sum . map (extractValue . processBrackets [] . splitInLexemes "")

extractValue :: [Lexeme] -> Int
extractValue [Value x] = x
extractValue _ = 0

processBrackets :: [Lexeme] -> [Lexeme] -> [Lexeme]
processBrackets acc [] = processMultiple . processAdd $ acc
processBrackets acc (OpenParen : xs) = processBrackets acc . processBrackets [] $ xs
processBrackets acc (CloseParen : xs) = (processMultiple . processAdd $ acc) ++ xs
processBrackets acc (x : xs) = processBrackets (acc ++ [x]) xs

processAdd :: [Lexeme] -> [Lexeme]
processAdd [] = []
processAdd (CloseParen : xs) = xs
processAdd (Value a : LexOp Add : Value b : xs) = processAdd $ Value (a + b) : xs
processAdd (x : xs) = x : processAdd xs

processMultiple :: [Lexeme] -> [Lexeme]
processMultiple [] = []
processMultiple (CloseParen : xs) = xs
processMultiple (Value a : LexOp Mult : Value b : xs) = processMultiple $ Value (a * b) : xs
processMultiple (x : xs) = x : processMultiple xs

-- This is my very broken AST generation, I still think
-- it's the better route. I think the best thing to do
-- Is convert into a [Processing] where
-- data Processing = PExpr Expr | PLex Lexeme
-- and iterate rules like (Value:Mult:Value) -> Expr
-- until the list is one expression item, if this
-- question comes back up I'll have to give that a go.

-- data Expr = ExprEmpty | ExprAtom Int | ExprOp Op Expr Expr
--   deriving (Show, Eq)

-- precedence :: Expr -> Int
-- precedence (ExprOp o _ _) = opPrecedence o
-- precedence _ = 0

-- opPrecedence :: Op -> Int
-- opPrecedence Add = 2
-- opPrecedence Mult = 3

-- generateAST :: Int -> Expr -> [Lexeme] -> (Expr, Expr, [Lexeme])
-- generateAST _ e [] = (e, ExprEmpty)
-- generateAST n ExprEmpty (Value x : xs) = generateAST n (ExprAtom x) xs
-- generateAST _ e (CloseParen : xs) = (e, ExprEmpty, xs)
-- generateAST currPrec e ((LexOp o) : xs)
--   | currPrec <= opPrecedence o = (\(rhs, rest) -> generateAST (ExprOp o e rhs) rest) $ generateAST (precedence o) ExprEmpty xs
--   | otherwise = (e, xs)
