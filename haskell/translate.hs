-- Exercise 1, chapter 9

-- from Bartosz Milewski's Basics of Haskell tutorial
-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
import qualified Data.Map as Map

type Dict = Map.Map String String

translate :: Dict -> String -> String
translate dict str = Map.findWithDefault "whatchamacallit" str dict

main =
    let
        dict = Map.fromList [("where", "dove"), ("is", "e"), ("the", "il")]
        dict' = Map.insert "colosseum" "colosseo" dict
    in do
        putStrLn $ unwords (map (translate dict) (words "where is the colosseum"))
        putStrLn $ unwords (map (translate dict') (words "where is the colosseum"))
