import Data.Char
import Data.List

data Huffman = Folha Int Char | No Int Huffman Huffman

freqSimb :: String -> [Huffman]
freqSimb str = countFreq (group (sort str))
  where
    countFreq [] = []
    countFreq (x : xs) = Folha (length x) (head x) : countFreq xs

construirArvore [x] = x
construirArvore xs = construirArvore (mergeFreqs (sortFreqs xs))

sortFreqs :: [Huffman] -> [Huffman]
sortFreqs = sortOn getFreq

mergeFreqs :: [Huffman] -> [Huffman]
mergeFreqs [] = []
mergeFreqs [x] = [x]
mergeFreqs (x : y : xs) = mergeFreqs (insertBy compareFreq (mergeNodes x y) xs)

getFreq :: Huffman -> Int
getFreq (Folha freq _) = freq
getFreq (No freq _ _) = freq

compareFreq :: Huffman -> Huffman -> Ordering
compareFreq h1 h2 = compare (getFreq h1) (getFreq h2)

mergeNodes :: Huffman -> Huffman -> Huffman
mergeNodes h1 h2 = No (getFreq h1 + getFreq h2) h1 h2

-- codHuffman :: Huffman -> [(Char, String)]

instance Show Huffman where
  show (Folha num c) = c : " " ++ show num
  show (No _ l r) = "(" ++ show l ++ show r ++ ")"

main :: IO ()
main = do
  print (freqSimb "adsadaaav")
