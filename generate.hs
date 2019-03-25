module Main where

import Control.Monad.State
import System.Environment
import System.Random

data Dir = L | R
    deriving (Show, Eq, Enum)

type RNG a = State StdGen a

randomNode :: RNG Int          -- ^ Depth distribution.
           -> (Int -> RNG Dir) -- ^ Direction distribution, depending on the distance to leaf.
           -> RNG [Dir]
randomNode depth dir = depth >>= mapM dir . reverse . enumFromTo 1

randomRS :: (Random a) => (a, a) -> RNG a
randomRS = state . randomR

treeDepth :: Double -> Int -> RNG Int
treeDepth coef limit = level <$> randomRS (0, last levels)
  where
    levels  = scanl (+) 0.0 . take limit $ iterate (* coef) 1.0
    level x = length (takeWhile (< x) levels) - 1

direction :: Double -> RNG Dir
direction leftChance = toEnum . fromEnum . (< leftChance) <$> randomRS (0, 1)

leanDir :: Double -> Int -> RNG Dir
leanDir d level
    | d <= 0    = direction factor
    | otherwise = direction (1 - factor)
  where
    factor = 0.5 * (1 / (1 + abs d)) ^ level

fingerRepresentation :: [[Dir]] -> [[Int]]
fingerRepresentation xs = zipWith diff ([]:xs) xs
  where
    diff a b = map (const 0) (drop prefix a) ++ map ((+1) . fromEnum) (drop prefix b)
      where
        prefix = length . takeWhile id $ zipWith (==) a b

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
        finger = concatMap (++ [value]) $ fingerRepresentation nodes
    putStr . unlines . map show $ depth:finger
