import Data.List (sortBy)
import Data.Map (Map, empty, insert, lookup, singleton, toList, union)
import Data.Maybe (fromJust, fromMaybe, isNothing)

-- Datatype representing a node in a Huffman Tree
data HuffmanNode
  = Leaf Int Char
  | Node Int HuffmanNode HuffmanNode
  deriving (Show)

main = do
  encodeArchive "input.txt" "bin.txt"
  decodeArchive "bin.txt" "output.txt"

encodeArchive :: String -> String -> IO ()
encodeArchive input output = do
  message <- readFile input
  let tree = computeHuffmanTreeFromString message
  let encoded = huffmanEncode message
  let serializedTree = serialize tree
  writeFile output (serializedTree ++ "\n" ++ encoded)

decodeArchive :: String -> String -> IO ()
decodeArchive input output = do
  contents <- readFile input
  let (serializedTree : encoded : _) = lines contents
  let tree = deserialize serializedTree
  let decoded = huffmanDecode encoded tree
  writeFile output decoded

huffmanEncode :: String -> String
huffmanEncode message = concatMap (\c -> fromJust (Data.Map.lookup c huffmanMap)) message
  where
    huffmanMap = huffmanTreeToMap (computeHuffmanTreeFromString message)

huffmanDecode :: String -> HuffmanNode -> String
huffmanDecode toDecode tree = huffmanDecodeLoop [] toDecode tree tree

huffmanDecodeLoop :: String -> String -> HuffmanNode -> HuffmanNode -> String
huffmanDecodeLoop result (x : xs) root (Node _ lChild rChild)
  | x == '0' = huffmanDecodeLoop result xs root lChild
  | x == '1' = huffmanDecodeLoop result xs root rChild
huffmanDecodeLoop result (x : xs) root (Leaf _ char) =
  huffmanDecodeLoop (result ++ [char]) (x : xs) root root
huffmanDecodeLoop result [] root node =
  case node of
    Leaf _ char -> result ++ [char]
    _ -> error "Expected a leaf node"

nodeValue :: HuffmanNode -> Int
nodeValue (Leaf val _) = val
nodeValue (Node val _ _) = val

leafChar :: HuffmanNode -> Char
leafChar (Leaf _ val) = val

leftChild :: HuffmanNode -> HuffmanNode
leftChild (Node _ child _) = child

rightChild :: HuffmanNode -> HuffmanNode
rightChild (Node _ _ child) = child

computeHuffmanTreeFromString :: String -> HuffmanNode
computeHuffmanTreeFromString message = buildHuffmanTree (occurrencesToNodes (countCharOccurrence message))

huffmanTreeToMap :: HuffmanNode -> Map Char String
huffmanTreeToMap node = huffmanTreeToMapLoop node []

huffmanTreeToMapLoop :: HuffmanNode -> String -> Map Char String
huffmanTreeToMapLoop (Node _ lChild rChild) string = Data.Map.union mapLeft mapRight
  where
    mapLeft = huffmanTreeToMapLoop lChild (string ++ "0")
    mapRight = huffmanTreeToMapLoop rChild (string ++ "1")
huffmanTreeToMapLoop (Leaf _ char) string = Data.Map.singleton char string

buildHuffmanTree :: [HuffmanNode] -> HuffmanNode
buildHuffmanTree nodes
  | length nodes == 1 = head nodes
  | otherwise =
    buildHuffmanTree (drop 2 (insertNode 0 (combineNodes (head nodes) (nodes !! 1)) nodes))

insertNode :: Int -> HuffmanNode -> [HuffmanNode] -> [HuffmanNode]
insertNode index node nodes
  | index <= 0 =
    insertNode 1 node nodes
  | nodeValue (nodes !! (index - 1)) >= nodeValue node =
    let (ys, zs) = splitAt (index - 1) nodes in ys ++ [node] ++ zs
  | index >= length nodes =
    nodes ++ [node]
  | otherwise =
    insertNode (index + 1) node nodes

combineNodes :: HuffmanNode -> HuffmanNode -> HuffmanNode
combineNodes nodeA nodeB = Node (nodeValue nodeA + nodeValue nodeB) nodeA nodeB

occurrencesToNodes :: [(Char, Int)] -> [HuffmanNode]
occurrencesToNodes = map (\x -> Leaf (snd x) (fst x))

compareOccurrences :: (Char, Int) -> (Char, Int) -> Ordering
compareOccurrences (k1, v1) (k2, v2)
  | v1 > v2 = GT
  | v1 < v2 = LT
  | v1 == v2 = compare k1 k2

countCharOccurrence :: String -> [(Char, Int)]
countCharOccurrence message =
  sortBy compareOccurrences (toList (countCharOccurrenceLoop message empty))

countCharOccurrenceLoop :: String -> Map Char Int -> Map Char Int
countCharOccurrenceLoop [] map = map
countCharOccurrenceLoop (x : xs) map
  | isNothing entry = countCharOccurrenceLoop xs (insert x 1 map)
  | otherwise = countCharOccurrenceLoop xs (insert x (fromJust entry + 1) map)
  where
    entry = Data.Map.lookup x map

serialize :: HuffmanNode -> String
serialize (Leaf _ c) = '1' : c : []
serialize (Node _ left right) = '0' : serialize left ++ serialize right

deserialize :: String -> HuffmanNode
deserialize s = fst (deserializeHelper s)
  where
    deserializeHelper ('1' : c : xs) = (Leaf 0 c, xs)
    deserializeHelper ('0' : xs) =
      let (left, xs') = deserializeHelper xs
          (right, xs'') = deserializeHelper xs'
       in (Node 0 left right, xs'')