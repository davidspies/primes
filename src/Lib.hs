module Lib where

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x : xs') ys@(y : ys')
  | y < x = y : merge xs ys'
  | otherwise = x : merge xs' ys

fmerge :: Ord a => [a] -> [a] -> [a]
fmerge [] ys       = ys
fmerge (x : xs) ys = x : merge xs ys

fmergePrefix :: Ord a => Int -> [[a]] -> ([a], [[a]])
fmergePrefix _ [] = ([], [])
fmergePrefix 1 (x : xs) = (x, xs)
fmergePrefix n xs = (fmerge y z, zs)
  where
    (y, ys) = fmergePrefix (n `quot` 2) xs
    (z, zs) = fmergePrefix ((n + 1) `quot` 2) ys

fmergeAll :: Ord a => [[a]] -> [a]
fmergeAll = go 1
  where
    go _ [] = []
    go n xs = let (y, ys) = fmergePrefix n xs in fmerge y (go (n * 2) ys)

excluding :: Ord a => [a] -> [a] -> [a]
excluding [] _ = []
excluding xs [] = xs
excluding xs@(x : xs') ys@(y : ys') =
  case compare x y of
    LT -> x : xs' `excluding` ys
    EQ -> xs' `excluding` ys'
    GT -> xs `excluding` ys'

primes :: [Integer]
primes = 2 : [3..] `excluding` composites

composites :: [Integer]
composites = fmergeAll [[p * p, p * (p + 1)..] | p <- primes]

multiplesOf :: [Integer] -> [Integer]
multiplesOf xs = fmergeAll [[x, x * 2..] | x <- xs]

rollWheel :: Int -> [Integer]
rollWheel nGenPrimes
  | nGenPrimes <= 0 = primes
  | otherwise = genPrimes ++ wheelPrimes
  where
    genPrimes :: [Integer]
    genPrimes = take nGenPrimes primes
    lcmgen :: Integer
    lcmgen = product genPrimes
    spokes :: [Integer]
    spokes = [1..lcmgen] `excluding` multiplesOf genPrimes
    -- Exclude 1
    wpCandidates :: [Integer]
    wpCandidates = tail $ (+) <$> [0, lcmgen..] <*> spokes
    wheelPrimes :: [Integer]
    wheelPrimes =
      head wpCandidates : tail wpCandidates `excluding` wheelComposites
    compositesFrom :: Integer -> [Integer]
    compositesFrom p = dropWhile (< p * p) $
        (+) <$> [m * start, m * (start + 1)..] <*> map (p *) spokes
      where
        m = p * lcmgen
        start = p `quot` lcmgen
    wheelComposites :: [Integer]
    wheelComposites = fmergeAll $ map compositesFrom wheelPrimes
