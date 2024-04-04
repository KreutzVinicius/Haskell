
main :: IO ()
main = do
  putStrLn "remove tree"
  let tree = NodeBT 10 (NodeBT 5 LeafBT LeafBT) (NodeBT 20 LeafBT LeafBT)
  let removedTree = remove 10 tree
  printBinaryTree removedTree

  putStrLn "\n\n"
  putStrLn "insert RB"
  let rbTree = NodeRB 8 B(NodeRB 5 R LeafRB LeafRB) (NodeRB 20 R LeafRB LeafRB)
  let newTree = insertRB 10 rbTree
  printRBTree newTree



data BinaryTree a = NodeBT a (BinaryTree a) (BinaryTree a) | LeafBT deriving Show

printBinaryTree :: Show a => BinaryTree a -> IO ()
printBinaryTree LeafBT = putStrLn "Leaf"
printBinaryTree (NodeBT x l r) = do
  putStrLn $ "Node " ++ show x
  putStrLn "Left subtree:"
  printBinaryTree l
  putStrLn "Right subtree:"
  printBinaryTree r

printRBTree :: Show a => RBTree a -> IO ()
printRBTree LeafRB = putStrLn "Leaf"
printRBTree (NodeRB x color left right) = do
  putStrLn $ "Node " ++ show x ++ " (Color: " ++ show color ++ ")"
  putStrLn "Left subtree:"
  printRBTree left
  putStrLn "Right subtree:"
  printRBTree right


remove :: Ord a => a -> BinaryTree a -> BinaryTree a
remove _ LeafBT = LeafBT
remove e (NodeBT x l r)
  | e == x = removeNode (NodeBT x l r)
  | e < x = NodeBT x (remove e l) r
  | e > x = NodeBT x l (remove e r)

removeNode :: Ord a => BinaryTree a -> BinaryTree a
removeNode (NodeBT _ LeafBT r) = r
removeNode (NodeBT _ l LeafBT) = l
removeNode (NodeBT _ l r) = NodeBT minRight l (removeMin r)
  where
    minRight = findMin r
    removeMin (NodeBT _ LeafBT r') = r'
    removeMin (NodeBT _ l' r') = NodeBT minRight l' (removeMin r')

findMin :: BinaryTree a -> a
findMin (NodeBT x LeafBT _) = x
findMin (NodeBT _ l _) = findMin l



----- 

data RB = R | B deriving Show
data RBTree a = LeafRB | NodeRB a RB (RBTree a) (RBTree a) deriving Show

-- Função de inserção em uma Red-Black Tree
insertRB :: Ord a => a -> RBTree a -> RBTree a
insertRB x tree = makeBlack (ins x tree)
  where
    ins :: Ord a => a -> RBTree a -> RBTree a
    ins e LeafRB = NodeRB e R LeafRB LeafRB
    ins e (NodeRB y color left right)
      | e < y = balance color (ins e left) y right
      | e > y = balance color left y (ins e right)
      | otherwise = NodeRB y color left right

    makeBlack :: RBTree a -> RBTree a
    makeBlack (NodeRB x _ left right) = NodeRB x B left right
    makeBlack LeafRB = LeafRB

    balance :: Ord a => RB -> RBTree a -> a -> RBTree a -> RBTree a
    balance B (NodeRB z R (NodeRB y R a b) c) x d = NodeRB y R (NodeRB z B a b) (NodeRB x B c d)
    balance B (NodeRB z R a (NodeRB y R b c)) x d = NodeRB y R (NodeRB z B a b) (NodeRB x B c d)
    balance B a x (NodeRB z R (NodeRB y R b c) d) = NodeRB y R (NodeRB x B a b) (NodeRB z B c d)
    balance B a x (NodeRB z R b (NodeRB y R c d)) = NodeRB y R (NodeRB x B a b) (NodeRB z B c d)
    balance color left x right = NodeRB x color left right


