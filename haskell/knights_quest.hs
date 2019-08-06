-- Knight's quest
-- example from chapter 12 of LYAH

-- ex: runhaskell --ghc-arg="-package sort" haskell/knights_quest.hs

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


main = do
    putStrLn (show (nub $ sort $ in3 (1,2)))
