module Main where

import Control.Monad.Loops (iterateM_)


import Lib
main :: IO ()
main = do
  h0 <- genesisHash
  print h0
  h1 <- nextHash "" h0
  print h1

  -- How to apply iterate :: (a -> a) -> a -> [a].

  -- We have:

  -- (take 20 $ iterate ((nextHash "") =<<) genesisHash) :: [IO Hash]

  -- mapM_ print :: (Show a, Foldable t) => t a -> IO ()

  -- so with t = [], a = Hash that's [Hash] -> IO ()
