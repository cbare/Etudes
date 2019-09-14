-- Simple Interpreter with state

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
envLookup x []         = unitM Wrong
envLookup x ((y,b):e)  = if x==y then unitM b else envLookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = tick `bindM` (\() -> unitM (Num (i+j)))
add a b             = unitM Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = tick `bindM` (\() -> k a)
apply f a       = unitM Wrong

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

-- a function that takes state and returns a result and a new state
type M a              = State -> (a, State)
type State            = Int

unitM a               = \s0 -> (a, s0)
m `bindM` k           = \s0 -> let (a,s1) = m s0
                                   (b,s2) = k a s1
                               in  (b,s2)


showM                :: M Value -> String
showM m               = let (a,s1) = m 0 in
                        "Value:" ++ showval a ++ "; " ++ "Count: " ++ show s1

tick                 :: M ()
tick                  = \s -> ((), s+1)

test                 :: Term -> String
test t                = showM (interp t [])


term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 10) (Con 11)))

term1 = (App (Lam "x" (Add (Var "x") (Var "bloorp")))
             (Add (Con 10) (Con 11)))

-- try to apply an Integer Constant, which is not a function
term2 = (App (Con 1234)
             (Add (Con 10) (Con 11)))


main = do
  putStrLn "Interpreter with state"
  putStrLn $ test term0
  putStrLn $ test term1
  putStrLn $ test term2
