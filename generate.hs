module Main where

import Control.Monad.State
import System.Environment
import System.Random

data Dir = L | R
    deriving (Show, Eq, Enum)

type RNG a = State StdGen a

-- 'randomNode depth dir' picks a random element from a tree.
--
-- Depth of the element is specified by 'depth'.
-- Left-to-right position is specified by 'dir'.
--
-- Note that the position distribution can depend on the depth to the leaf,
-- giving an opportunity to use differently weighted choices on each step
-- of the path from root to the chosen depth level.
--
-- >>> evalState (randomNode (randomRS (5,9)) (\_ -> direction 0.5)) (mkStdGen 0)
-- [R,R,L,L,L,R,L,L]
--
-- >>> evalState (randomNode (randomRS (1,5)) (\_ -> direction 0.9)) (mkStdGen 0)
-- [R,R,R,R]
--
randomNode :: RNG Int          -- Depth distribution.
           -> (Int -> RNG Dir) -- Direction distribution, depending on the distance to leaf.
           -> RNG [Dir]
randomNode depth dir = depth >>= mapM dir . reverse . enumFromTo 1

-- 'randomR', wrapped in the state monad.
randomRS :: (Random a) => (a, a) -> RNG a
randomRS = state . randomR

-- 'treeDepth coef limit' picks a random depth level from a tree.
--
-- 'coef' specifies the branching factor of the tree.
-- 'limit' specifies the maximum depth.
--
-- Higher branching factor increases the probability of picking
-- depth levels closer to the bottom, since they contain more
-- elements.
--
-- >>> evalState (replicateM 10 (treeDepth 0.1 10)) (mkStdGen 0)
-- [1,0,0,0,0,1,0,0,0,0]
--
-- >>> evalState (replicateM 10 (treeDepth 4 10)) (mkStdGen 0)
-- [9,8,7,9,9,9,9,9,9,9]
--
treeDepth :: Double -> Int -> RNG Int
treeDepth coef limit = level <$> randomRS (0, last levels)
  where
    levels  = scanl (+) 0.0 . take limit $ iterate (* coef) 1.0
    level x = length (takeWhile (< x) levels) - 1

-- Bernoulli distribution.
--
-- >>> evalState (replicateM 10 (direction 0.5)) (mkStdGen 0)
-- [L,R,R,L,L,L,L,L,R,L]
--
-- >>> evalState (replicateM 10 (direction 1)) (mkStdGen 0)
-- [R,R,R,R,R,R,R,R,R,R]
--
direction :: Double -> RNG Dir
direction leftChance = toEnum . fromEnum . (< leftChance) <$> randomRS (0, 1)

-- 'leanDir lean level' picks a random direction.
--
-- 'lean' specifies the prefered direction, negative values represent
-- left side, positive values represent right side. Higher absolute value
-- increases the likelihood of picking the prefered direction.
--
-- 'level' represents the tree depth. Levels near the root of the tree are
-- weighted towards the prefered direction much more.
--
-- >>> evalState (replicateM 10 (leanDir 0 1)) (mkStdGen 0)
-- [L,R,R,L,L,L,L,L,R,L]
--
-- >>> evalState (replicateM 10 (leanDir 1 1)) (mkStdGen 0)
-- [L,R,R,R,L,L,L,R,R,L]
--
-- >>> evalState (replicateM 10 (leanDir 1 10)) (mkStdGen 0)
-- [R,R,R,R,R,R,R,R,R,R]
--
leanDir :: Double -> Int -> RNG Dir
leanDir d level
    | d <= 0    = direction factor
    | otherwise = direction (1 - factor)
  where
    factor = 0.5 * (1 / (1 + abs d)) ^ level

-- Convert a list of element positions within a tree to the
-- corresponding zipper movements.
--
-- >>> zipperRepresentation [[L], [R,R]]
-- [[1],[0,2,2]]
--
zipperRepresentation :: [[Dir]] -> [[Int]]
zipperRepresentation xs = zipWith diff ([]:xs) xs
  where
    diff a b = map (const 0) (drop prefix a) ++ map ((+1) . fromEnum) (drop prefix b)
      where
        prefix = length . takeWhile id $ zipWith (==) a b

-- Replacement value.
value :: Int
value = 3

main :: IO ()
main = do
    s <- getStdGen
    [c, d, l, b] <- getArgs
    let count  = read c  -- Number of random elements.
        depth  = read d  -- Depth of the tree.
        lean   = read l  -- Direction distribution factor. Negative values represent left side, positive values
                         --   right side. Higher absolute value is more likely to produce the prefered direction.
        branch = read b  -- Branch factor. Higher values are more likely to select nodes near the bottom of the tree.
        nodes  = (`evalState` s) . replicateM count $ randomNode (treeDepth branch depth) (leanDir lean)
        finger = concatMap (++ [value]) $ zipperRepresentation nodes
    putStr . unlines . map show $ depth:finger
