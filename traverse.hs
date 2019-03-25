{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Main where

import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Criterion.Main
import qualified Data.ByteString.Char8 as BS
import           Data.Int
import           Data.STRef.Unboxed
import qualified Data.Vector.Unboxed as V
import           System.Mem
import           System.IO

-- Binary tree.
data Tree
    = Node !Tree !Int64 !Tree
    | Leaf
    deriving (Show, Eq)

-- Path from the root to the focus.
data Path
    = PathLeft  !Int64 !Tree  !Path
    | PathRight !Tree  !Int64 !Path
    | Nil
    deriving (Show, Eq)

-- Binary tree zipper.
data Zipper
    = Zipper !Tree !Int64 !Tree !Path
    deriving (Show, Eq)

-- Construct a full binary tree of the given depth.
--
-- >>> full 2
-- Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)
--
full :: Int64 -> Tree
full = go 1
  where
    go _ 0 = Leaf
    go v n = Node (go (2 * v) (n - 1)) v (go (2 * v + 1) (n - 1))

-- Create a zipper focused at the root of the tree.
--
-- The tree must not be empty.
--
-- >>> zipperStart (full 2)
-- Zipper (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf) Nil
--
zipperStart :: Tree -> Zipper
zipperStart Leaf         = error "zipperStart"
zipperStart (Node l x r) = Zipper l x r Nil

-- Move the focus to the parent node. If already at the
-- root, do nothing.
--
-- >>> zipperUp (Zipper Leaf 1 Leaf (PathLeft 2 Leaf Nil))
-- Zipper (Node Leaf 1 Leaf) 2 Leaf Nil
--
zipperUp :: Zipper -> Zipper
zipperUp (Zipper ll lx lr (PathLeft  x r p)) = Zipper (Node ll lx lr) x r p
zipperUp (Zipper rl rx rr (PathRight l x p)) = Zipper l x (Node rl rx rr) p
zipperUp z                                   = z

-- Move the focus to the left subtree. If the left subtree is empty,
-- do nothing.
--
-- >>> zipperLeft . zipperStart $ full 2
-- Zipper Leaf 2 Leaf (PathLeft 1 (Node Leaf 3 Leaf) Nil)
--
zipperLeft :: Zipper -> Zipper
zipperLeft (Zipper (Node ll lx lr) x r p) = Zipper ll lx lr (PathLeft x r p)
zipperLeft z                              = z

-- Move the focus to the right subtree. If the right subtree is empty,
-- do nothing.
--
-- >>> zipperRight . zipperStart $ full 2
-- Zipper Leaf 3 Leaf (PathRight (Node Leaf 2 Leaf) 1 Nil)
--
zipperRight :: Zipper -> Zipper
zipperRight (Zipper l x (Node rl rx rr) p) = Zipper rl rx rr (PathRight l x p)
zipperRight z                              = z

-- Change the value of the focused element.
--
-- >>> zipperSet 2 (Zipper Leaf 1 Leaf Nil)
-- Zipper Leaf 2 Leaf Nil
--
zipperSet :: Int64 -> Zipper -> Zipper
zipperSet x (Zipper l _ r p) = Zipper l x r p

-- 'whileM c f x' applies 'f' to 'x' while the condition
-- 'c' holds.
--
-- >>> (`evalState` 1) (whileM ((<=10) <$> get) (\x -> state $ \s -> (s*x, s+1)) 1)
-- 3628800
--
whileM :: (Monad m) => m Bool -> (a -> m a) -> a -> m a
whileM c f = go
  where
    go a = do
        c' <- c
        if c'
            then f a >>= go
            else return a

-- Returns 'True' for numbers that specify zipper movement.
--
-- >>> map isControl [-1..3]
-- [False,True,True,True,False]
--
isControl :: Int64 -> Bool
isControl i = i >= 0 && i <= 2

-- Convert the tree to a zipper, apply all the zipper operations
-- specified by the vector and convert back to a tree.
--
-- >>> interpretZipper (V.fromList [1,100,0,200]) (full 2)
-- Node (Node Leaf 100 Leaf) 200 (Node Leaf 3 Leaf)
--
interpretZipper :: V.Vector Int64 -> Tree -> Tree
interpretZipper !v !t = top $ V.foldl' go (zipperStart t) v
  where
    go x 0 = zipperUp x
    go x 1 = zipperLeft x
    go x 2 = zipperRight x
    go x y = zipperSet y x

    top (Zipper l x r Nil) = Node l x r
    top z                  = top (zipperUp z)

-- Apply all tree operations specified by the vector.
-- This function uses the strict state monad.
--
-- >>> interpretRootState (V.fromList [1,100,2,200]) (full 2)
-- Node (Node Leaf 100 Leaf) 1 (Node Leaf 200 Leaf)
--
interpretRootState :: V.Vector Int64 -> Tree -> Tree
interpretRootState !v !t = evalState (whileM ((< V.length v) <$> get) branch t) 0
  where
    branch Leaf = error "interpretRootState"
    branch (Node l x r) = do
        ix <- state (\s -> (s, s + 1))
        case v `V.unsafeIndex` ix of
            0  -> error "interpretRootState"
            1  -> (\l' -> Node l' x r ) <$> branch l
            2  -> (\r' -> Node l  x r') <$> branch r
            x' -> return $ Node l x' r

-- Same as above, except this function uses the ST monad.
interpretRootST :: V.Vector Int64 -> Tree -> Tree
interpretRootST !v !t = runST $ do
    !i <- newSTRefU 0
    let branch Leaf = error "interpretRootST"
        branch (Node l x r) = do
            ix <- readSTRefU i
            writeSTRefU i (ix + 1)
            case v `V.unsafeIndex` ix of
                0  -> error "interpretRootST"
                1  -> (\l' -> Node l' x r ) <$> branch l
                2  -> (\r' -> Node l  x r') <$> branch r
                x' -> return $ Node l x' r
    whileM ((< V.length v) <$> readSTRefU i) branch t

-- Same as above, except this function uses an auxiliary vector
-- constructed with 'findIndices'.
interpretRootIndices :: V.Vector Int64 -> Tree -> Tree
interpretRootIndices !v !t = go 0 $ branch 0 t
  where
    !ixs = V.findIndices (not . isControl) v
    !len = V.length ixs

    go i x
        | i < len - 1 = go (i + 1) (branch (1 + ixs `V.unsafeIndex` i) x)
        | otherwise   = x

    branch _  Leaf = error "interpretRootIndices"
    branch ix (Node l x r) = case v `V.unsafeIndex` ix of
        0  -> error "interpretRootIndices"
        1  -> Node (branch (ix + 1) l) x r
        2  -> Node l x (branch (ix + 1) r)
        x' -> Node l x' r

-- Same as above, except this function finds the next instruction
-- sequence by using 'findIndex'.
interpretRootIndex :: V.Vector Int64 -> Tree -> Tree
interpretRootIndex !v !t = go 0 t
  where
    go s x = case V.findIndex (not . isControl) (V.unsafeDrop s v) of
        Nothing -> x
        Just ix -> go (s + ix + 1) (branch s x)

    branch _  Leaf = error "interpretRootIndex"
    branch ix (Node l x r) = case v `V.unsafeIndex` ix of
        0  -> error "interpretRootIndex"
        1  -> Node (branch (ix + 1) l) x r
        2  -> Node l x (branch (ix + 1) r)
        x' -> Node l x' r

-- A variant of interpretRootIndices, where the auxiliary vector is part of
-- the input. The function does not need to calculate it.
interpretRootPrecomputed :: V.Vector Int -> V.Vector Int64 -> Tree -> Tree
interpretRootPrecomputed !ixs !v !t = go 0 $ branch 0 t
  where
    !len = V.length ixs

    go i x
        | i < len - 1 = go (i + 1) (branch (1 + ixs `V.unsafeIndex` i) x)
        | otherwise   = x

    branch _  Leaf = error "interpretRootPrecomputed"
    branch ix (Node l x r) = case v `V.unsafeIndex` ix of
        0  -> error "interpretRootPrecomputed"
        1  -> Node (branch (ix + 1) l) x r
        2  -> Node l x (branch (ix + 1) r)
        x' -> Node l x' r

data TestCase = Case !(V.Vector Int64) !Tree
    deriving (Show, Eq)

-- Convert zipper movement instructions into node positions
-- relative to the root.
--
-- >>> convert (V.fromList [1,100,0,2,200])
-- [1,100,2,200]
--
convert :: V.Vector Int64 -> V.Vector Int64
convert = V.fromList . reverse . snd . V.foldl' go ([], [])
  where
    go (s@ ~(_:s'), r) x = case x of
        0 -> (s',  r)
        1 -> (1:s, r)
        2 -> (2:s, r)
        _ -> (s,   x:s ++ r)

-- Read the depth of the tree and all instructions from the given
-- handle.
--
-- Returns the depth, the zipper instructions and the tree instructions.
loadHandle :: Handle -> IO (Int64, V.Vector Int64, V.Vector Int64)
loadHandle h = do
    d:i <- map unsafeRead . BS.lines <$> BS.hGetContents h
    let !v1 = V.fromList i
        !v2 = convert v1
    return (d, v1, v2)
  where
    unsafeRead bs = case BS.readInt bs of
        Just (i, _) -> fromIntegral i
        Nothing     -> error "loadHandle"

-- Runs a single 'TestCase'.
run :: (V.Vector Int64 -> Tree -> Tree) -> TestCase -> Tree
run f (Case v t) = f v t

main :: IO ()
main = do
    (d, z, r) <- loadHandle stdin
    let !tree         = full d
        !ixs          = V.findIndices (not . isControl) r
        !testCaseZip  = Case z tree
        !testCaseRoot = Case r tree
    performGC
    defaultMain
        [ bgroup "interpretRoot"
            [ bench "state"       $ whnf (run interpretRootState            ) testCaseRoot
            , bench "ST"          $ whnf (run interpretRootST               ) testCaseRoot
            , bench "indices"     $ whnf (run interpretRootIndices          ) testCaseRoot
            , bench "index"       $ whnf (run interpretRootIndex            ) testCaseRoot
            , bench "precomputed" $ whnf (run (interpretRootPrecomputed ixs)) testCaseRoot
            ]
        , bgroup "interpretZipper"
            [ bench "zipper" $ whnf (run interpretZipper) testCaseZip
            ]
        ]
