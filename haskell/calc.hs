-- symbolic calculator

-- from Bartosz Milewski's Basics of Haskell tutorial
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
import Control.Monad (unless)
import Data.Char

data Token = TokOp Operator
           | TokIdent String
           | TokNum Double
           | TokAssign
           | TokLParen
           | TokRParen
           | TokEnd
    deriving (Show, Eq)

data Expression

data Operator = Plus | Minus | Times | Div deriving (Read, Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
  deriving Show


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


-- Tokenizer ---------------------------------------------------------

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | elem c "+-*/" = TokOp (operator c) : tokenize cs
    | isDigit c     = number c cs
    | isAlpha c     = identifier c cs
    | c == '='      = TokAssign : tokenize cs
    | c == '('      = TokLParen : tokenize cs
    | c == ')'      = TokRParen : tokenize cs
    | isSpace c     = tokenize cs
    | otherwise     = error $ "Cannot tokenize " ++ [c]


identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in
                  TokIdent (c:str) : tokenize cs'


number :: Char -> String -> [Token]
number c cs =
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenize cs'


-- Parser ------------------------------------------------------------

-- Grammar for parsing expressions
-- Expression <- Term [+-] Expression
--             | Identifier '=' Expression
--             | Term
-- Term       <- Factor [*/] Term
--             | Factor
-- Factor     <- Number
--             | Identifier
--             | [+-] Factor
--             | '(' Expression ')'

parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
             in
               if null toks'
               then tree
               else error $ "Leftover tokens: " ++ show toks'


expression :: [Token] -> (Tree, [Token])
expression toks =
   let (termTree, toks') = term toks
   in
      case lookAhead toks' of
         -- Term [+-] Expression
         (TokOp op) | elem op [Plus, Minus] ->
            let (exTree, toks'') = expression (accept toks')
            in (SumNode op termTree exTree, toks'')
         -- Identifier '=' Expression
         TokAssign ->
            case termTree of
               VarNode str ->
                  let (exTree, toks'') = expression (accept toks')
                  in (AssignNode str exTree, toks'')
               _ -> error "Only variables can be assigned to"
         -- Term
         _ -> (termTree, toks')


term :: [Token] -> (Tree, [Token])
term toks =
   let (facTree, toks') = factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = term (accept toks')
            in (ProdNode op facTree termTree, toks'')
         _ -> (facTree, toks')


factor :: [Token] -> (Tree, [Token])
factor toks =
   case lookAhead toks of
      (TokNum x)     -> (NumNode x, accept toks)
      (TokIdent str) -> (VarNode str, accept toks)
      (TokOp op) | elem op [Plus, Minus] ->
            let (facTree, toks') = factor (accept toks)
            in (UnaryNode op facTree, toks')
      TokLParen      ->
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks


lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c


accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts


evaluate :: Expression -> Double
evaluate = undefined

main :: IO ()
main = do
    putStrLn "Calc - Enter an expression to be evaluated:"
    line <- getLine
    unless (line == ":q") $ do
        print $ (parse . tokenize) line
        main
