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

rle :: Eq a => [a] -> [(a, Int)]
rle xs = map (\(gs@(g:_)) -> (g, length gs)) (pack xs)