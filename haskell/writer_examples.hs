-- Monadic Writer examples from LYAH chapt 13

-- ex: runhaskell --ghc-arg="-package mtl" haskell/log_writer.hs
import Control.Monad.Writer


logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])


multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)


gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)


main = do
    putStrLn "\nMultiply two numbers"
    let (x, log) = runWriter multWithLog
    mapM_ putStrLn log
    putStrLn $ replicate 60 '-'
    putStrLn $ show x

    putStrLn "\n\nFinding GCD"
    let (x, log) = runWriter $ gcd' (8*11*3*7) (8*3*5*2)
    mapM_ putStrLn log
    putStrLn $ replicate 60 '-'
    putStrLn $ show x
    
