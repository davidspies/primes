module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  [k, n] <- map read <$> getArgs
  print $ rollWheel k !! (n - 1)
