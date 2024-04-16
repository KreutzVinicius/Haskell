import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord

data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree deriving (Show)

-- Função auxiliar para contar a frequência dos caracteres em uma string
countFrequency :: String -> Map.Map Char Int
countFrequency = Map.fromListWith (+) . map (\c -> (c, 1))

-- Função para construir a árvore de Huffman
buildHuffmanTree :: Map.Map Char Int -> HuffmanTree
buildHuffmanTree freqMap = buildTree $ map (\(c, f) -> (Leaf c f, f)) $ Map.toList freqMap
  where
    buildTree [(_, f)] = Leaf '\0' f
    buildTree xs =
      let (a, b) = splitAt 2 $ sortBy (comparing snd) xs
          node = Node (snd (head a) + snd (head b)) (buildTree a) (buildTree b)
       in buildTree $ insertBy (comparing snd) (node, 0) (tail b)

-- Função para gerar a tabela de codificação
generateEncodingTable :: HuffmanTree -> Map.Map Char String
generateEncodingTable tree = Map.fromList $ generateCodes "" tree
  where
    generateCodes _ (Leaf c _) = [(c, "")]
    generateCodes code (Node _ left right) = generateCodes (code ++ "0") left ++ generateCodes (code ++ "1") right

-- Função para compactar uma string usando a codificação de Huffman
compress :: String -> String
compress input = concatMap (\c -> Map.findWithDefault "" c encodingTable) input
  where
    freqMap = countFrequency input
    huffmanTree = buildHuffmanTree freqMap
    encodingTable = generateEncodingTable huffmanTree

-- Função para descompactar uma string usando a codificação de Huffman
decompress :: String -> String
decompress input = go input huffmanTree ""
  where
    huffmanTree = buildHuffmanTree $ countFrequency input
    go [] _ acc = acc
    go (x : xs) (Leaf c _) acc = go (x : xs) huffmanTree (acc ++ [c])
    go (x : xs) (Node _ left right) acc = if x == '0' then go xs left acc else go xs right acc

-- Função principal
main :: IO ()
main = do
  -- Nome do arquivo de entrada
  let inputFile = "./text.txt"
  inputContent <- readFile inputFile
  putStrLn inputContent

  -- Nome do arquivo descompactado
  let decompressedFile = "./text-descompactado.txt"

  let compressed = compress inputContent
  putStrLn compressed

  -- Nome do arquivo compactado
  let compressedFile = "./text.bin"
  writeFile compressedFile compressed
  compressedData <- readFile compressedFile

  -- Descompactar o conteúdo
  let decompressed = decompress compressedData
  putStrLn decompressed

  -- Escrever o conteúdo descompactado no arquivo de saída
  writeFile decompressedFile decompressed

  putStrLn "Compactação e descompactação concluídas com sucesso!"
