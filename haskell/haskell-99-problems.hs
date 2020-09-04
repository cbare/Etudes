-- haskell 99 problems

-- 1.
last' (x:[]) = x
last' (x:xs) = last' xs

-- 2.
lastButOne [] = Nothing
lastButOne (y:[]) = Nothing
lastButOne (x:y:[]) = Just x
lastButOne (x:xs) = lastButOne xs

-- 3. kth element of a list
kth _ [] = Nothing
kth 1 (x:xs) = Just x
kth k (x:xs) = kth (k-1) xs

-- 4.
len [] = 0
len (x:xs) = 1 + len xs

-- 5.
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

-- 5b.
reverse' l = rev l []
  where 
    rev [] a = a
    rev (x:xs) a = rev xs (x:a)

-- 6.
isPalendrome xs = xs == reverse' xs

-- 7.
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldl (++) [] (map flatten xs)

-- 8.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:ys@(y:_)) = if x==y then (compress ys) else x:(compress ys)

compress :: Eq a => [a] -> [a]
compress (x:ys@(y:_)) = if x==y then (compress ys) else x:(compress ys)
compress ys = ys

-- 9.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = let (tail, grp) = (split_ xs []) in grp:(pack tail)
  where
    split_ (xs@(y:ys)) [] = split_ ys [y]
    split_ [] gs = ([], gs)
    split_ (xs@(y:ys)) gs@(g:_)
      | y==g = (split_ ys (gs ++ [y]))
      | otherwise = (xs, gs)

-- 10.
rlencode :: Eq a => [a] -> [(a, Int)]
rlencode xs = map (\(gs@(g:_)) -> (g, length gs)) (pack xs)

-- 12.
rldecode :: Eq a => [(a, Int)] -> [a]
rldecode [] = []
rldecode ((x,n):xs) = (replicate n x) ++ (rleDecode xs)

-- 14.
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- 15.
repli n [] = []
repli n (x:xs) = (replicate n x) ++ (repli n xs)

repli :: Int -> [a] -> [a]
repli n xs = concatMap (replicate n) xs

repli :: Int -> [a] -> [a]
repli n xs = xs >>= replicate n

-- 16.
dropEveryKth k xs = dropEveryKth_ k 1 xs
  where
    dropEveryKth_ k i [] = []
    dropEveryKth_ k i (x:xs) =
      if (i `mod` k == 0)
        then dropEveryKth_ k (i+1) xs
        else x:(dropEveryKth_ k (i+1) xs)

-- 17.
split :: Int -> [a] -> ([a], [a])
split n (x:xs) | n > 0 = (x:ys, zs)
  where (ys, zs) = split (n-1) xs
split _ xs = ([], xs)

-- 18.
slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice i j (x:xs)
  | i > 1 = slice (i-1) (j-1) xs
  | j > 0 = x:(slice (i-1) (j-1) xs)
  | otherwise = []

-- 19.
rotate :: Int -> [a] -> [a]
rotate n xs
  | n >= 0 = (drop m xs) ++ (take m xs)
  | otherwise = rotate ((length xs)+m) xs
    where
      m = n `mod` (length xs)
