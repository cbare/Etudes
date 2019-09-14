-- Fibonacci numbers

fibonacci :: Int -> Integer
fibonacci n | n < 0      = undefined
            | n == 0     = 0
            | n == 1     = 1
            | otherwise  = fibonacci (n-1) + fibonacci (n-2)

showFibonacci :: Int -> String
showFibonacci n = "fibonacci(" ++ (show n) ++ ") = " ++ (show . fibonacci) n

showFibonacciNums :: String -> String
showFibonacciNums = unlines .
    map (showFibonacci . read) .
    lines

main = interact showFibonacciNums
