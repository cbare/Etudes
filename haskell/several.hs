import Data.Char

type Parser a = String -> (a, String)

several :: Parser a -> String -> [a]
several parse  "" = []
several parse str = let (s, str') = parse str
                      in s : several parse str'

num :: Parser Int
num str = let (x, str') = span isDigit str
              (_, str'') = span isSpace str'
           in (read x, str'')

main = print $ several num "12 4 128"
