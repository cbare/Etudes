data WhyNot a = Nah | Sure a
  deriving Show

instance Functor WhyNot where
    fmap f (Sure a) = Sure (f a)
    fmap f Nah      = Nah

instance Applicative WhyNot where
    (<*>) (Sure f) (Sure a) = Sure (f a)
    (<*>) Nah _             = Nah
    (<*>) (Sure f) Nah      = Nah
    pure a = Sure a

instance Monad WhyNot where
    (>>=) (Sure a) f = (f a)
    (>>=) Nah f      = Nah
    return a         = Sure a
    fail _           = Nah

safeRoot :: Double -> WhyNot Double
safeRoot x = 
    if x >= 0 then 
      return (sqrt x)
    else
      fail "Boo!"

test :: Double -> WhyNot Double
test x = do
   y <- safeRoot x
   z <- safeRoot (y - 4)
   w <- safeRoot z
   return w


main = do
    print $ test 9
    print $ test 400