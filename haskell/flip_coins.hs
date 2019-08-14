-- flip coins
-- probablity example from Chapt 13 of LYAH

import Data.List (all)
import Data.Ratio
import qualified Data.Map as Map

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show


instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs


instance Applicative Prob where
  pure x = Prob [(x,1%1)]
  Prob [(x,r)] <*> prob = fmap x prob


flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs


consolidate :: (Ord a) => Prob a -> Prob a
consolidate (Prob xs) = Prob $ Map.toList $ Map.fromListWith (+) xs


-- question: could we use this instead?
-- consolidate :: (Ord a, Num b) => [(a,b)] -> [(a,b)]
-- consolidate xs = Map.toList $ Map.fromListWith (+) xs


instance Monad Prob where
    m >>= f = flatten (fmap f m)
    fail _ = Prob []


data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]


loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]


flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])


main = do
    print $ getProb $ consolidate $ flipThree
