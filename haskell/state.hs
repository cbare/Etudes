-- State monad example

-- see: https://vaibhavsagar.com/blog/2016/06/17/haskell-state/

-- ex: runhaskell --ghc-arg="-package random" haskell/state.hs

{-# language InstanceSigs #-}

import System.Random


newtype State s a = State (s -> (a, s))


instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap fn (State sa) = State (\s0 -> let (a, s1) = sa s0 in (fn a, s1))


instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (State sa) (State sb) =
    State (\s0 -> let (fn, s1) = sa s0
                      (a,  s2) = sb s1
                   in (fn a, s2))


instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State h) >>= k = State $ \s0 -> let (a, s1) = h s0
                                       State g = k a
                                    in (g s1)

addRandom x = State $ \g -> let (a,g') = randomR (0,100) g
                             in (a+x, g')

mulRandom x = State $ \g -> let (a,g') = randomR (0,100) g
                             in (a*x, g')

runRandom input g = let (State f) = (pure input >>= mulRandom >>= addRandom)
                     in f g

main = do
  let (a, g1) = runRandom (1::Int) (mkStdGen 0)
   in putStrLn $ "1st Answer: " ++ (show a)

