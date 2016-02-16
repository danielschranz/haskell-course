module Golf where
import Data.List
import qualified Data.Map as M
----------------------- ex 1
skip :: [a] -> Int -> [a]
skip [] _ = []
skip s n = helper (drop n s)        -- this is just to drop the first n elements, from then on helper always keeps the first element of the sequence
    where helper :: [a] -> [a]
          helper [] = []
          helper (x:xs) = x : helper (drop n xs)

skips :: [a] -> [[a]]
skips x = map (skip x) [0.. (pred . length) x]

----------------------- ex 2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = (convertToValues . getLocalMaximaIndices) [1.. (length xs - 2)]
    where convertToValues = map ((!!) xs)
          getLocalMaximaIndices = filter isIndexLocalMaxima
          isIndexLocalMaxima :: Int -> Bool
          isIndexLocalMaxima n = isSmallerThanPredecessor && isSmallerThanSuccessor
                where isSmallerThanPredecessor = xs !! pred n < xs !! n
                      isSmallerThanSuccessor = xs !! succ n < xs !! n

histogram :: [Integer] ->  String
histogram xs = if null xs
  then intercalate "\n" epilogue
  else intercalate "\n" (graph ++ epilogue)
    where graph = map line [maxOccurs,pred maxOccurs..1]
          maxOccurs = head . reverse. sort . map length. group . sort $ xs
          frequencies = M.fromList (map (\x -> (head x, length x)) . group . sort $ xs)
          epilogue = ["==========", "0123456789\n"]
          line :: Int -> String
          line x = map (\i -> if (M.findWithDefault 0 i frequencies) >= x then '*' else ' ') [0..9]
