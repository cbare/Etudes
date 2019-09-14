-- Simple Interpreter with error messages

-- from The essence of functional programming by Philip Wadler

type Name = String

data Term   = Var Name
            | Con Int
            | Add Term Term
            | Lam Name Term
            | App Term Term

data Value  = Wrong
            | Num Int
            | Fun (Value -> M Value)

type Environment    = [(Name, Value)]


showval :: Value -> String
showval Wrong = "Wrong"
showval (Num i) = show i
showval (Fun f) = "<function>"

envLookup :: Name -> Environment -> M Value
envLookup x [] = errorM ("unbound variable: " ++ x)
envLookup x ((y,b):e)  = if x==y then unitM b else envLookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add a b = errorM ("adding applies to numbers: " ++ showval a ++ ", " ++ showval b)

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a = errorM ("applying non-function: " ++ showval f)

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

-- Error messages
data M a = Success a | Error String
unitM a = Success a
errorM s = Error s
(Success a) `bindM` k = k a
(Error s) `bindM` k = Error s

showM :: M Value -> String
showM (Success a) = "Success: " ++ showval a
showM (Error s) = "Error: " ++ s


test :: Term -> String
test t          = showM (interp t [])


term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 10) (Con 11)))

term1 = (App (Lam "x" (Add (Var "x") (Var "bloorp")))
             (Add (Con 10) (Con 11)))

-- try to apply an Integer Constant, which is not a function
term2 = (App (Con 1234)
             (Add (Con 10) (Con 11)))


main = do
  putStrLn "Interpreter with error messages"
  putStrLn $ test term0
  putStrLn $ test term1
  putStrLn $ test term2
