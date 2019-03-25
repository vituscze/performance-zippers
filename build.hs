{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Main where

import Criterion.Main
import Data.Int

-- Redundant 2-3 tree.
data Tree
    = Leaf
    | Node2 !Tree !Int64 !Tree
    | Node3 !Tree !Int64 !Tree !Int64 !Tree
    deriving (Show, Eq)

-- The focus of the zipper.
data Nonempty
    = Nonempty2 !Tree !Int64 !Tree
    | Nonempty3 !Tree !Int64 !Tree !Int64 !Tree
    deriving (Show, Eq)

-- A single choice on the path from the root to the focus.
data PathChoice
    = Path2L {- -} !Int64 !Tree
    | Path2R !Tree !Int64 {- -}
    | Path3L {- -} !Int64 !Tree !Int64 !Tree
    | Path3M !Tree !Int64 {- -} !Int64 !Tree
    | Path3R !Tree !Int64 !Tree !Int64 {- -}
    deriving (Show, Eq)

-- Path from the root to the focus.
data Path = Nil | Cons !PathChoice !Path
    deriving (Show, Eq)

-- 2-3 tree zipper.
data Zipper = Zipper !Nonempty !Path
    deriving (Show, Eq)

data Dir = L | R
    deriving (Show, Eq)

-- Create a 2-3 tree containing a single element.
--
-- >>> singleton 1
-- Node2 Leaf 1 Leaf
--
singleton :: Int64 -> Tree
singleton v = Node2 Leaf v Leaf

-- Create a 2-3 tree zipper containing a single element.
--
-- >>> singletonZ 1
-- Zipper (Nonempty2 Leaf 1 Leaf) Nil
--
singletonZ :: Int64 -> Zipper
singletonZ v = Zipper (Nonempty2 Leaf v Leaf) Nil

-- Reconstruct the 2-3 tree from a zipper.
--
-- This function moves the focus to the root and then
-- reconstructs the root node.
--
-- >>> top (singletonZ 1)
-- Node2 Leaf 1 Leaf
--
top :: Zipper -> Tree
top (Zipper node path) = case node of
    Nonempty2 l x r     -> go path (Node2 l x r)
    Nonempty3 l x m y r -> go path (Node3 l x m y r)
  where
    go Nil        !t = t
    go (Cons c p) !t = go p (fill t c)

    fill l (Path2L x r)     = Node2 l x r
    fill r (Path2R l x)     = Node2 l x r
    fill l (Path3L x m y r) = Node3 l x m y r
    fill m (Path3M l x y r) = Node3 l x m y r
    fill r (Path3R l x m y) = Node3 l x m y r

-- Result of a tree insert operation.
data Inserted = Done !Tree | Split !Tree !Int64 !Tree

-- Insert a value into a 2-3 tree. If the value is
-- already present in the tree, do nothing.
--
-- >>> insertTree 0 Leaf
-- Node2 Leaf 0 Leaf
--
-- >>> foldr insertTree Leaf [1..5]
-- Node3 (Node2 Leaf 1 Leaf) 2 (Node3 Leaf 2 Leaf 3 Leaf) 4 (Node3 Leaf 4 Leaf 5 Leaf)
--
insertTree :: Int64 -> Tree -> Tree
insertTree !v !t = case go t of
    Done d      -> d
    Split l x r -> Node2 l x r
  where
    go Leaf = Split Leaf v Leaf

    -- Data node insert.
    go (Node2 Leaf x _)
        | v < x     = Done (Node3 Leaf v Leaf x Leaf)
        | v == x    = Done (Node2 Leaf v Leaf)
        | otherwise = Done (Node3 Leaf x Leaf v Leaf)
    go (Node3 Leaf x _ y _)
        | v < x     = Split (Node2 Leaf v Leaf) x (Node3 Leaf x Leaf y Leaf)
        | v == x    = Done (Node3 Leaf v Leaf y Leaf)
        | v < y     = Split (Node2 Leaf x Leaf) v (Node3 Leaf v Leaf y Leaf)
        | v == y    = Done (Node3 Leaf x Leaf v Leaf)
        | otherwise = Split (Node2 Leaf x Leaf) y (Node3 Leaf y Leaf v Leaf)

    -- Internal node insert.
    go (Node2 l x r)
        | v < x     = case go l of
            Done l'        -> Done (Node2 l' x r)
            Split ll v' lr -> Done (Node3 ll v' lr x r)
        | otherwise = case go r of
            Done r'        -> Done (Node2 l x r')
            Split rl v' rr -> Done (Node3 l x rl v' rr)
    go (Node3 l x m y r)
        | v < x     = case go l of
            Done l'        -> Done (Node3 l' x m y r)
            Split ll v' lr -> Split (Node2 ll v' lr) x (Node2 m y r)
        | v < y     = case go m of
            Done m'        -> Done (Node3 l x m' y r)
            Split ml v' mr -> Split (Node2 l x ml) v' (Node2 mr y r)
        | otherwise = case go r of
            Done r'        -> Done (Node3 l x m y r')
            Split rl v' rr -> Split (Node2 l x m) y (Node2 rl v' rr)

-- Insert a value into 2-3 tree zipper. If the value is already
-- present, do nothing. Returns a zipper focused on the node
-- containing the new value.
--
-- Note that this function assumes that the focus is already in the
-- correct position.
--
-- >>> insertZipper 1 (singletonZ 0)
-- Zipper (Nonempty3 Leaf 0 Leaf 1 Leaf) Nil
--
-- >>> foldr insertZipper (singletonZ 0) [1,2]
-- Zipper (Nonempty3 Leaf 1 Leaf 2 Leaf) (Cons (Path2R (Node2 Leaf 0 Leaf) 1) Nil)
--
insertZipper :: Int64 -> Zipper -> Zipper
insertZipper !v (Zipper node path) = case node of
    Nonempty2 _ x _
        | v < x     -> Zipper (Nonempty3 Leaf v Leaf x Leaf) path
        | v == x    -> Zipper (Nonempty2 Leaf v Leaf) path
        | otherwise -> Zipper (Nonempty3 Leaf x Leaf v Leaf) path
    Nonempty3 _ x _ y _
        | v < x     -> Zipper (Nonempty2 Leaf v Leaf) (go x (Node3 Leaf x Leaf y Leaf) L path)
        | v == x    -> Zipper (Nonempty3 Leaf v Leaf y Leaf) path
        | v < y     -> Zipper (Nonempty3 Leaf v Leaf y Leaf) (go v (Node2 Leaf x Leaf) R path)
        | v == y    -> Zipper (Nonempty3 Leaf x Leaf v Leaf) path
        | otherwise -> Zipper (Nonempty3 Leaf y Leaf v Leaf) (go y (Node2 Leaf x Leaf) R path)
  where
    go i !t L Nil = Cons (Path2L i t) Nil
    go i !t R Nil = Cons (Path2R t i) Nil

    go i !t L (Cons (Path2L x r) p) = Cons (Path3L i t x r) p
    go i !t R (Cons (Path2L x r) p) = Cons (Path3M t i x r) p
    go i !t L (Cons (Path2R l x) p) = Cons (Path3M l x i t) p
    go i !t R (Cons (Path2R l x) p) = Cons (Path3R l x t i) p

    go i !t L (Cons (Path3L x m y r) p) = Cons (Path2L i t) (go x (Node2 m y r) L p)
    go i !t R (Cons (Path3L x m y r) p) = Cons (Path2R t i) (go x (Node2 m y r) L p)
    go i !t L (Cons (Path3M l x y r) p) = Cons (Path2R l x) (go i (Node2 t y r) L p)
    go i !t R (Cons (Path3M l x y r) p) = Cons (Path2L y r) (go i (Node2 l x t) R p)
    go i !t L (Cons (Path3R l x m y) p) = Cons (Path2L i t) (go y (Node2 l x m) R p)
    go i !t R (Cons (Path3R l x m y) p) = Cons (Path2R t i) (go y (Node2 l x m) R p)

-- Generate a tree by inserting all natural numbers less than or equal to 'start'
-- in descending order.
--
-- >>> generate 4
-- Node3 (Node2 Leaf 0 Leaf) 1 (Node3 Leaf 1 Leaf 2 Leaf) 3 (Node3 Leaf 3 Leaf 4 Leaf)
--
generate :: Int64 -> Tree
generate !start = go start (singleton start)
  where
    go !v !t
        | v > 0     = go (v - 1) (insertTree (v - 1) t)
        | otherwise = t

-- Same as above except that zipper is used as the intermediate structure.
--
-- >>> generateZ 4
-- Node3 (Node2 Leaf 0 Leaf) 1 (Node3 Leaf 1 Leaf 2 Leaf) 3 (Node3 Leaf 3 Leaf 4 Leaf)
--
generateZ :: Int64 -> Tree
generateZ !start = top $ go start (singletonZ start)
  where
    go !v !z
        | v > 0     = go (v - 1) (insertZipper (v - 1) z)
        | otherwise = z

main :: IO ()
main = defaultMain
    [ bgroup "tree"
        [ bench "10000000" $ whnf generate 10000000
        ]
    , bgroup "zipper"
        [ bench "10000000" $ whnf generateZ 10000000
        ]
    ]
