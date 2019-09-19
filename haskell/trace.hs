newtype Trace a = Trace ([String], a)

instance Functor Trace where
  fmap k (Trace (msgs, x)) = Trace (msgs, (k x))

instance Applicative Trace where
  (<*>) (Trace (msgs', k)) (Trace (msgs'', x)) = Trace (msgs' ++ msgs'', (k x))
  pure x = Trace ([], x)

instance Monad Trace where
    (>>=) (Trace (msgs, x)) k = 
      let Trace (new_msgs, y) = k x
       in Trace (msgs ++ new_msgs, y)
    return x = Trace ([], x)
    

put :: Show a => String -> a -> Trace ()
put msg v = Trace ([msg ++ " " ++ show v], ())

fact :: Integer -> Trace Integer
fact n = do
   put "fact" n
   if n == 0
       then return 1
       else do
           m <- fact (n - 1)
           return (n * m)

main = let Trace (lst, m) = fact 3
       in do
           print lst
           print m