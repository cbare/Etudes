
-- quicksort, but not in-place
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) =
    let lower = quicksort [x | x <- xs, x <= pivot]
        upper = quicksort [x | x <- xs, x > pivot]
    in  lower ++ [pivot] ++ upper


-- merge two sorted lists into a single sorted list
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge l1@(x:xs) l2@(y:ys)
    | x <= y     = x:(merge xs l2)
    | otherwise  = y:(merge l1 ys) 

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort l =
    let (xs, ys) = splitAt ((length l) `quot` 2) l
    in merge (mergesort xs) (mergesort ys)
