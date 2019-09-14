-- parse nested parens into a tree structure

-- Exercise 1 from chapter 8 of Basics of Haskell
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser

data Token = TokLParen | TokRParen | TokEnd
    deriving (Show, Eq)

lookAhead :: String -> Token
lookAhead [] = TokEnd
lookAhead (c:cs)| c == '(' = TokLParen
                | c == ')' = TokRParen
                | otherwise = error $ "Bad input: " ++ (c:cs)

accept :: String -> String
accept [] = error "Nothing to accept"
accept (c:cs) = cs

data Tree = Node Tree Tree | Leaf
    deriving Show

root, expr, par :: String -> (Tree, String)

root = par

expr    s = let (p, s')   = par s 
                (p', s'') = par s'
            in (Node p p', s'')

par     s = case lookAhead s of
                TokLParen ->
                    case lookAhead (accept s) of
                        TokRParen -> (Leaf, accept (accept s))
                        _         -> let (e, s') = expr (accept s)
                                     in
                                        if lookAhead s' == TokRParen
                                            then (e, accept s')
                                            else error $ "Missing closing paren in: " ++ show s'

                _ -> error $ "Bad expression: " ++ show s

parse str = let (tree, str') = root str
            in
                if null str' 
                then tree 
                else error $ "Unconsumed string " ++ str'

main = print $ parse "(()(()(()())))"
