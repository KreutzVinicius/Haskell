{- REPRESENTATION CONVENTION:
     - Red represents the color red.
     - Black represents the color black.
   REPRESENTATION INVARIANT:
     True
 -}
data Color = Red | Black
  deriving (Show)

{- REPRESENTATION CONVENTION:
     - Void represents an empty red-black tree.
     - Node c x l r represents a non-empty red-black tree with root node color
       c, root label x, left subtree l and right subtree r.
   REPRESENTATION INVARIANT:
     Every red-black tree is a binary search tree, and
     no red node has a red parent, and
     every path from the root to an empty subtree contains the same number of
       black nodes.
 -}
data RBTree = Void | Node Color Int RBTree RBTree
  deriving (Show)

{- insert t v
   PRE: True
   POST: if v exists in t, then t, else t with v inserted (rebalanced as
     necessary to preserve the red-black invariants)
 -}
insert :: RBTree -> Int -> RBTree
insert t v =
  let ins Void = Node Red v Void Void
      ins (Node c x l r)
        | x == v = Node c x l r
        | v < x = balance c x (ins l) r
        | x < v = balance c x l (ins r)
      Node _ x l r = ins t
   in Node Black x l r

{- balance c x l r
   PRE: True
   POST: the tree Node c x l r, rebalanced as necessary to preserve the
     red-black invariants
 -}
balance :: Color -> Int -> RBTree -> RBTree -> RBTree
balance Black z (Node Red y (Node Red x a b) c) d =
  Node Red y (Node Black x a b) (Node Black z c d)
balance Black z (Node Red x a (Node Red y b c)) d =
  Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red z (Node Red y b c) d) =
  Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red y b (Node Red z c d)) =
  Node Red y (Node Black x a b) (Node Black z c d)
balance c x l r = Node c x l r