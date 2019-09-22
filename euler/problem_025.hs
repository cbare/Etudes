-- Euler problem 25

-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

-- infinite sequence of fibonacci numbers
fibs = fibs' 0 1
    where fibs' x y = y : fibs' y (x + y)

firstWithLengthN n seq =
    let (i,x):_ = take 1 $ dropWhile (\(i, x) -> length (show x) < n) (zip [1..] seq)
     in (i,x)

main = print $ firstWithLengthN 1000 fibs
