-- Monadic Log Writer example from LYAH chapt 14

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


main = do
    let (x, log) = runWriter multWithLog
    mapM_ putStrLn log
    putStrLn $ replicate 60 '-'
    putStrLn $ show x

