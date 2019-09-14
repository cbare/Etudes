-- split a sentence into words

-- Exercise 2 from chapter 8 of Basics of Haskell
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8_Parser

import Prelude hiding (Word)
import Data.Char

type Word = String

sentence :: String -> [Word]
sentence  "" = []
sentence str = let (w, str') = word str
                in w : sentence str'

-- returns a word and the rest of input
word :: String -> (Word, String)
word str = let (w, str') = span (not . isSpace) str
               (_, str'') = span isSpace str'
            in (w, str'')

main = print $ sentence "Ceci n'est pas une phrase"
