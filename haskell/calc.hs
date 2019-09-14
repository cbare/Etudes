-- symbolic calculator

-- from Bartosz Milewski's Basics of Haskell tutorial
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
import Control.Monad (unless)
import Data.Char

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
    deriving (Show, Eq)

data Expression

data Operator = Plus | Minus | Times | Div deriving (Read, Show, Eq)


opToChar :: Operator -> Char
opToChar Plus  = '+'
opToChar Minus = '-'
opToChar Times = '*'
opToChar Div   = '/'

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/" = TokOp (operator c) : tokenize cs
    | isDigit c  = TokNum (digitToInt c) : tokenize cs
    | isAlpha c  = TokIdent [c]          : tokenize cs
    | isSpace c  = tokenize cs
    | otherwise  = error $ "Cannot tokenize " ++ [c]

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

main :: IO ()
main = do
    putStrLn "Calc - Enter an expression to be evaluated:"
    line <- getLine
    unless (line == "quit") $ do
        print $ tokenize line
        main
