-- Exercise 2 from chapter 9 of Basics of Haskell
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

opToStr :: Operator -> String
opToStr Plus  = " + "
opToStr Minus = " - "
opToStr Times = " * "
opToStr Div   = " / "

paren :: Tree -> String
paren (VarNode str)      = str
paren (NumNode x)        = show x
paren (UnaryNode op t)   = "(" ++ (opToStr op) ++ paren t ++ ")"
paren (AssignNode str t) = "(" ++ str ++ " = " ++ paren t ++ ")"
paren (ProdNode op t s)  = "(" ++ paren t ++ (opToStr op) ++ paren s ++ ")"
paren (SumNode op t s)   = "(" ++ paren t ++ (opToStr op) ++ paren s ++ ")"

-- x = 2 * (y = 5) + 3
testExpr = AssignNode "x" (SumNode Plus 
                             (ProdNode Times 
                               (NumNode 2.0) 
                               (AssignNode "y" (NumNode 5)))
                             (NumNode 3))

main = do
  print $ paren testExpr
