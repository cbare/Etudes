-- Simple Interpreter with output

-- from The essence of functional programming by Philip Wadler

type Name = String

data Term   = Var Name
            | Con Int
            | Add Term Term
            | Lam Name Term
            | App Term Term
            | Out Term

data Value  = Wrong
            | Num Int
            | Fun (Value -> M Value)

type Environment    = [(Name, Value)]

showval :: Value -> String
showval Wrong = "Wrong"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e    = envLookup x e
interp (Con i) e    = unitM (Num i)
interp (Add u v) e  = interp u e `bindM` (\a ->
                        interp v e `bindM` (\b ->
                          add a b))
interp (Lam x v) e  = unitM (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e  = interp t e `bindM` (\f ->
                        interp u e `bindM` (\a ->
                          apply f a))
interp (Out u) e    = interp u e `bindM` (\a ->
                        outO a   `bindM` (\() ->
                          unitM a))

envLookup :: Name -> Environment -> M Value
envLookup x []         = unitM Wrong
envLookup x ((y,b):e)  = if x==y then unitM b else envLookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add a b             = unitM Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a       = unitM Wrong

test :: Term -> String
test t          = showM (interp t [])


type M a      = (String, a)
unitM a       = ("", a)
m `bindM` k   = let (r,a) = m
                    (s,b) = k a in
                    (r++s, b)
showM (s,a)   = "Output: " ++ s ++ " Value: " ++ showval a

outO          :: Value -> M ()
outO a        = (showval a ++ "; ", ())


term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 10) (Con 11)))

term1 = (App (Lam "x" (Add (Var "x") (Var "notfound")))
             (Add (Con 10) (Con 11)))

term2 = (Add (Out (Con 41)) (Out (Con 1)))


main = do
    putStrLn "Interpreter with output"
    putStrLn "\n\nTerm1"
    putStrLn $ test term0
    putStrLn "\n\nTerm2"
    putStrLn $ test term1
    putStrLn "\n\nTerm3"
    putStrLn $ test term2
