-- Project Euler Problem 098
-- find anagramic squares

-- Holy crap did I get geek-sniped. There went a Saturday.
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.Sort (sort)


-- strip quotes froms strings
sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
            | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
            | otherwise      = s
sq s                         = s


anagrams :: [String] -> Map.Map String [String]
anagrams [] = Map.empty
anagrams (w:ws) =
    Map.insertWith (++) (sort w) [w] (anagrams ws)


type Permutation = [Int]
type Anagram = (String, String)
type AnagremPerm = (Anagram, Permutation)


findIndex :: Int -> Char -> [Char] -> [Char] -> ([Char], Maybe Int)
findIndex i x prefix [] = (prefix, Nothing)
findIndex i x prefix (y:ys) = if x==y
    then ((prefix++['_']++ys), Just i)
    else findIndex (i+1) x (prefix++[y]) (ys)


-- O(n^2), but largest n is small
toPerm :: [Char] -> [Char] -> Permutation
toPerm [] w2     = []
toPerm (a:as) w2 = 
    let (w3, mi) = findIndex 0 a [] w2
     in case mi of
        Nothing -> error $ "can't find a " ++ (show a)
        Just i -> i : (toPerm as w3)


pairToPerm :: (Anagram) -> AnagremPerm
pairToPerm (w1,w2) = ((w1,w2), (toPerm w1 w2))

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- List.tails l, y <- ys]

permute :: Ord a => Permutation -> [a] -> [a]
permute p l = map snd $ sort $ zip p l


-- infinite list of squares
squares :: (Num a, Enum a) => [a]
squares = [x*x | x <- [1..]]

isSquare :: (Ord a, Num a, Enum a) => a -> Bool
isSquare n = 
    let x:_ = dropWhile (\x -> x < n) squares
     in x==n

-- list squares in order as strings while they are less than n digits long
squaresUpToLen :: Int -> [String]
squaresUpToLen n = takeWhile (\x -> (length x) <= n) $ (map show squares)


-- test whether a string of digits matches a pattern, for example
--   matches "SUCCESS" "1322411" returns True
--   matches "SUCCESS" "7322477" returns True
--   matches "ABBA" "1111" return False
--   matches "ABBCC" "1223" return False
matches :: String -> String -> Bool
matches pattern digits = check pattern digits Map.empty Set.empty 
    where
        check []       []   _ _    = True
        check (p:ps)   []   _ _    = False
        check []     (d:ds) _ _    = False
        check (p:ps) (d:ds) m seen = 
            if p `Map.member` m
                then (m Map.! p == d) && (check ps ds m seen)
                else (not (d `Set.member` seen)) && (check ps ds (Map.insert p d m) (Set.insert d seen))


-- find patterns matching digits, permute digits through the anagram, then filter
-- the results for squares
findAnagramicSquares :: [AnagremPerm] -> String -> [(Anagram, (String, String))]
findAnagramicSquares ps digits =
    filter (\results -> (isSquare . read) (snd (snd results)))
        (map (\p -> (fst p, (digits, permute (snd p) digits)))
             (filter (\p -> (matches (fst (fst p)) digits)) ps))


main = do
    txt <- readFile "p098_words.txt"
    let words = map sq $ splitOn "," txt
        m     = Map.filter (\ws -> (length ws) > 1) $ anagrams $ words
        maxl  = maximum (map length (Map.keys m))
        ps    = (map pairToPerm) $ concat $ Map.elems $ Map.map pairs m
     in mapM_ print $ concat $ map (findAnagramicSquares ps) $ squaresUpToLen maxl

