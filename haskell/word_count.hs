-- word counter
-- Exercise 3, chapter 9

-- from Bartosz Milewski's Basics of Haskell tutorial
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
import Data.Char (toLower)
import qualified Data.Map as Map

type Dict = Map.Map String Int

incr dict k =
    let n = Map.findWithDefault 0 k dict
     in Map.insert k (n+1) dict

count dict words =
    foldl incr dict words

text = "This is a big bag of words blah blah it is this \
        \a this bag of a bag of a bag this BIG bag is it \
        \foo bar bat biz quux bar quux foo quux big foo"

main = 
    let dict = Map.empty :: Dict
     in print $ count dict (words (map toLower text))
