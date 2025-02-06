module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                     = 0
tsize (Branch (size, _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                      = 0
tdepth (Branch (_, depth) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember value (Branch _ leftTree member rightTree) = case (compare value member) of
  LT -> tmember value leftTree
  EQ -> True
  GT -> tmember value rightTree

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch leftTree value rightTree = Branch ((tsize leftTree) + (tsize rightTree) + 1, (+) 1 $ max (tdepth leftTree) (tdepth rightTree)) leftTree value rightTree

trotateLeft :: Tree a -> Tree a
trotateLeft (Branch _ left value (Branch _ middle rvalue right)) = mkBranch (mkBranch left value middle) rvalue right
trotateLeft tree = tree

trotateRight :: Tree a -> Tree a
trotateRight (Branch _ (Branch _ left lvalue middle) value right) = mkBranch left lvalue (mkBranch middle value right)
trotateRight tree = tree

leftRotation :: Tree a -> a -> Tree a -> Tree a
leftRotation left value (Branch meta middle rvalue right)
  | (tdepth middle) > (tdepth right) = trotateLeft (mkBranch left value (trotateRight (Branch meta middle rvalue right)))
  | otherwise = trotateLeft (mkBranch left value (Branch meta middle rvalue right))
leftRotation left value right = mkBranch left value right

rightRotataion :: Tree a -> a -> Tree a -> Tree a
rightRotataion (Branch meta left lvalue middle) value right
  | (tdepth left) < (tdepth middle) = trotateRight (mkBranch (trotateLeft (Branch meta left lvalue middle)) value right)
  | otherwise = trotateRight (mkBranch (Branch meta left lvalue middle) value right)
rightRotataion right value left = mkBranch right value left

tbalance :: Tree a -> Tree a
tbalance Leaf = Leaf
tbalance (Branch meta left value right)
  | (tdepth left) + 1 < (tdepth right) = leftRotation left value right
  | (tdepth left) > (tdepth right) + 1 = rightRotataion left value right
  | otherwise = (Branch meta left value right)

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert value Leaf = mkBranch Leaf value Leaf
tinsert value (Branch meta leftTree member rightTree) = case (compare value member) of
  LT -> tbalance (mkBranch (tinsert value leftTree) member rightTree)
  EQ -> (Branch meta leftTree member rightTree)
  GT -> tbalance (mkBranch leftTree member (tinsert value rightTree))

tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf
