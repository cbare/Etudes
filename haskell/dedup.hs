-- deduplicate a list
import qualified Data.Set as Set

dedup :: Ord a => [a] -> [a]
dedup xs = _dedup xs Set.empty
  where
    _dedup (x:xs) seen
      | (Set.member x seen) = _dedup xs seen
      | otherwise           = x: _dedup xs (Set.insert x seen)
    _dedup xs seen = xs

main =
  do
    putStrLn $ dedup "abcdefgabcdefgaabbababa"
