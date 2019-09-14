-- Knight's quest
-- example from chapter 12 of LYAH

-- ex: runhaskell --ghc-arg="-package sort" haskell/knights_quest.hs

import Control.Monad ((<=<))
import Data.Sort (sort)
import Data.List (nub)


type KnightPos = (Int,Int)


valid :: KnightPos -> Bool
valid (c,r) = c `elem` [1..8] && r `elem` [1..8]


generateMoves :: KnightPos -> [KnightPos]
generateMoves (c,r) = [(c + fst m * a, r + snd m * b) |
    m <- [(1,2), (2,1)],
    a <- [1,-1],
    b <- [1,-1]]


moveKnight :: KnightPos -> [KnightPos]
moveKnight pos = filter valid $ generateMoves pos


in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight


canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start


inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)


canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start


main = do
    putStrLn "From (4,4) a knight can move to:"
    print $ moveKnight (4,4)

    putStrLn "From (1,1) a knight can move to:"
    print $ moveKnight (1,1)

    putStrLn "From (1,3) a knight can move to:"
    print $ moveKnight (1,3)

    putStrLn "Can reach in 3 moves"
    putStrLn (show (nub $ sort $ in3 (4,4)))

    putStrLn $ "Can reach in 1 moves: " ++ (show (length (nub $ sort $ inMany 1 (1,2))))
    putStrLn $ "Can reach in 2 moves: " ++ (show (length (nub $ sort $ inMany 2 (1,2))))
    putStrLn $ "Can reach in 3 moves: " ++ (show (length (nub $ sort $ inMany 3 (1,2))))
    putStrLn $ "Can reach in 4 moves: " ++ (show (length (nub $ sort $ inMany 4 (1,2))))
    putStrLn $ "Can reach in 5 moves: " ++ (show (length (nub $ sort $ inMany 5 (1,2))))
