-- Final Countdown
--   from chapter 14 of Learn you a Haskell

--   Demonstrates faster performance of DiffList which
--   represents a list as nested applications of concatenate

-- ex: runhaskell --ghc-arg="-package mtl" haskell/final_countdown.hs
import Control.Monad.Writer


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }


toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)


fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []


instance Show a => Show (DiffList a) where
    show d = "DiffList " ++ (show $ fromDiffList d)


instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))


instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)


finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])


stringCountDown :: Int -> Writer [String] ()
stringCountDown 0 = do
    tell ["0"]
stringCountDown x = do
    stringCountDown (x-1)
    tell [show x]

main = do
    putStrLn "Countdown from:"
    line <- getLine
    let n = (read line) :: Int
    mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown n
    mapM_ putStrLn . snd . runWriter $ stringCountDown n
