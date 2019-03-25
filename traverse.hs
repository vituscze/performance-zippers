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

data Tree
    = Node !Tree !Int64 !Tree
    | Leaf
    deriving (Show, Eq)

data Path
    = PathLeft  !Int64 !Tree  !Path
    | PathRight !Tree  !Int64 !Path
    | Nil
    deriving (Show, Eq)

data Zipper
    = Zipper !Tree !Int64 !Tree !Path
    deriving (Show, Eq)

full :: Int64 -> Tree
full = go 1
  where
    go _ 0 = Leaf
    go v n = Node (go (2 * v) (n - 1)) v (go (2 * v + 1) (n - 1))

zipperStart :: Tree -> Zipper
zipperStart Leaf         = error "zipperStart"
zipperStart (Node l x r) = Zipper l x r Nil

zipperUp :: Zipper -> Zipper
zipperUp (Zipper ll lx lr (PathLeft  x r p)) = Zipper (Node ll lx lr) x r p
zipperUp (Zipper rl rx rr (PathRight l x p)) = Zipper l x (Node rl rx rr) p
zipperUp z                                   = z

zipperLeft :: Zipper -> Zipper
zipperLeft (Zipper (Node ll lx lr) x r p) = Zipper ll lx lr (PathLeft x r p)
zipperLeft z                              = z

zipperRight :: Zipper -> Zipper
zipperRight (Zipper l x (Node rl rx rr) p) = Zipper rl rx rr (PathRight l x p)
zipperRight z                              = z

zipperSet :: Int64 -> Zipper -> Zipper
zipperSet x (Zipper l _ r p) = Zipper l x r p

whileM :: (Monad m) => m Bool -> (a -> m a) -> a -> m a
whileM c f = go
  where
    go a = do
        c' <- c
        if c'
            then f a >>= go
            else return a

isControl :: Int64 -> Bool
isControl i = i >= 0 && i <= 2

interpretZipper :: V.Vector Int64 -> Tree -> Tree
interpretZipper !v !t = top $ V.foldl' go (zipperStart t) v
  where
    go x 0 = zipperUp x
    go x 1 = zipperLeft x
    go x 2 = zipperRight x
    go x y = zipperSet y x

    top (Zipper l x r Nil) = Node l x r
    top z                  = top (zipperUp z)

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

-- Left fold version of interpretRoot with a precomputed vector of hints
-- (positions where the path from root starts).
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
convert :: V.Vector Int64 -> V.Vector Int64
convert = V.fromList . reverse . snd . V.foldl' go ([], [])
  where
    go (s@ ~(_:s'), r) x = case x of
        0 -> (s',  r)
        1 -> (1:s, r)
        2 -> (2:s, r)
        _ -> (s,   x:s ++ r)

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
