-- symbolic calculator

-- from Bartosz Milewski's Basics of Haskell tutorial
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
import Control.Monad (mapM_)
import Control.Monad.State
import Data.Char
import Data.List (dropWhileEnd)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)


type SymTab = Map.Map String Double

data Token = TokOp Operator
           | TokIdent String
           | TokNum Double
           | TokAssign
           | TokLParen
           | TokRParen
           | TokEnd
    deriving (Show, Eq)

data Expression

data Operator = Plus | Minus | Times | Div deriving (Eq)

instance Show Operator where
    show Plus  = " + "
    show Minus = " - "
    show Times = " * "
    show Div   = " / "

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String

instance Show Tree where
    show (VarNode str)      = str
    show (NumNode x)        = show x
    show (UnaryNode op t)   = "(" ++ (show op) ++ show t ++ ")"
    show (AssignNode str t) = "(" ++ str ++ " = " ++ show t ++ ")"
    show (ProdNode op t s)  = "(" ++ show t ++ (show op) ++ show s ++ ")"
    show (SumNode op t s)   = "(" ++ show t ++ (show op) ++ show s ++ ")"


operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


format :: (String, Double) -> String
format (k, v) = k ++ " = " ++ (show v)


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


-- Evaluator ---------------------------------------------------------

type Evaluator a = State SymTab a


evaluate :: Tree -> Evaluator Double

evaluate (SumNode op larg rarg) = do
    l <- evaluate larg
    r <- evaluate rarg
    case op of
        Plus  -> return (l + r)
        Minus -> return (l - r)

evaluate (ProdNode op larg rarg) = do
    l <- evaluate larg
    r <- evaluate rarg
    case op of
        Times -> return (l * r)
        Div   -> return (l / r)

evaluate (UnaryNode op arg) = do
    x <- evaluate arg
    case op of
        Plus  -> return x
        Minus -> return (-x)

evaluate (NumNode x) = return x

evaluate (AssignNode str tree) = do
    val  <- evaluate tree
    assign str val

evaluate (VarNode str) = lookupSymbol str


lookupSymbol :: String -> Evaluator Double
lookupSymbol str = do
    symbols <- get
    case Map.lookup str symbols of
      Just val -> return val
      Nothing  -> error $ "Undefined variable " ++ str


assign :: String -> Double -> Evaluator Double
assign str val = do
    symbols <- get
    put $ Map.insert str val symbols
    return val


main :: IO ()
main = do
    putStrLn "Calc - Enter an expression to be evaluated:"
    loop (Map.fromList [("pi", pi), ("e", exp 1)])


loop symbols = do
    line <- prompt "> "
    line <- return (trim line)
    case line of
        "" -> do
            loop symbols
        ":s" -> do
            mapM_ (putStrLn . format) (Map.toList symbols)
            loop symbols
        ":q" -> do
            putStrLn "Bye!"
        otherwise -> do
            let tree = (parse . tokenize) line
                (val, symbols') = runState (evaluate tree) symbols
             in do
                print val
                loop symbols'
