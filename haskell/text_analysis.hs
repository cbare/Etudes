-- Compute letter distribution in a text file

-- Examples:
--   runhaskell haskell/text_analysis.hs letter-counts --filepath /path/to/doc.txt
--   runhaskell haskell/text_analysis.hs letter-frequency --filepath /path/to/doc.txt


import qualified Data.Map as Map
import System.IO
import Data.Char (toLower)
import Options.Applicative
import Data.Semigroup ((<>))
import Numeric (showFFloat)

stringToLower = map toLower
keepLetters = filter (`elem` ['a'..'z'])

toCharIntPairs :: String -> [(Char, Int)]
toCharIntPairs = map (\c -> (c, 1))

-- Given a string, return a map from character to the number of times it appears in the string
letter_counts :: String -> Map.Map Char Int
letter_counts string = Map.fromListWith (+) ((toCharIntPairs . keepLetters . stringToLower) string)

pad :: (Show a) => Int -> a -> String
pad n x = replicate (n-l) ' ' ++ (show x)
        where l = (length . show) x

-- Format a map for output
format_counts :: Map.Map Char Int -> [String]
format_counts m = Map.foldrWithKey (\ k v ss -> ([k] ++ ": " ++ (pad w v)):ss) [] m
        where w = Map.foldr (\ x acc -> max acc ((length . show) x)) 0 m

letter_frequencies :: String -> Map.Map Char Double
letter_frequencies string = Map.map (\x -> fromIntegral(x)/fromIntegral(total)) counts
        where counts = letter_counts string
              total = (Map.foldr (+) 0 counts)


format_float x = showFFloat (Just 3) x ""

format_frequencies :: Map.Map Char Double -> [String]
format_frequencies m = Map.foldrWithKey (\ k v ss -> ([k] ++ ": " ++ (format_float v)):ss) [] m


-- Command line options via the Options.Applicative module
data Command = 
      LetterCount FilePath
    | LetterFrequency FilePath
    deriving (Eq, Show)

letterCountOptions :: Parser Command
letterCountOptions = LetterCount
      <$> strOption
          ( long "filepath"
         <> short 'f'
         <> metavar "FILEPATH"
         <> help "Path to text file" )

letterFrequencyOptions :: Parser Command
letterFrequencyOptions = LetterFrequency
      <$> strOption
          ( long "filepath"
         <> short 'f'
         <> metavar "FILEPATH"
         <> help "Path to text file" )

cmds :: Parser Command
cmds = subparser (
    command "letter-counts"   (info (letterCountOptions <**> helper) (progDesc "Count occurences of letters" ))
 <> command "letter-frequency" (info (letterFrequencyOptions <**> helper) (progDesc "Compute letter frequencies" )))

opts :: ParserInfo Command
opts = info (cmds <**> helper)
  ( fullDesc
  <> progDesc "Analyze a piece of text."
  <> header "Text Analyzer - get statistics on text." )

main = do
    cmd <- execParser opts

    case cmd of 
            LetterCount filepath     -> do
                                            text <- readFile filepath
                                            let x = letter_counts text
                                            mapM_ putStrLn (format_counts x)
            LetterFrequency filepath -> do
                                            text <- readFile filepath
                                            let x = letter_frequencies text
                                            mapM_ putStrLn (format_frequencies x)

    -- text <- readFile filepath
    -- let x = f text
    -- mapM_ putStrLn (format_map x)

