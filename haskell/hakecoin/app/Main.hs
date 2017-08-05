module Main where

import Lib

main :: IO ()
main = do
  h0 <- genesisHash
  print h0
  nextHash "" h0 >>= print
