{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import Text.Printf (printf)

hash = h1 where h = SHA256.init
                h1 = SHA256.update h "someFunc"

hexHash :: SHA256.Ctx -> String
hexHash = concatMap (printf "%02x") . B.unpack . SHA256.finalize

someFunc :: IO ()
someFunc = putStrLn $ hexHash hash
