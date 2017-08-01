{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , Hash
    ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import Text.Printf (printf)
import Data.DateTime (DateTime, getCurrentTime, toSqlString)

data Hash = Hash { index :: Int
                 , timestamp :: DateTime
                 , content :: String -- "data" in Snakecoin
                 , previousHash :: String
                 , hash :: String
                 } deriving (Show)

hashTest = h1 where h = SHA256.init
                    h1 = SHA256.update h "someFunc"

hexHash :: SHA256.Ctx -> String
hexHash = concatMap (printf "%02x") . B.unpack . SHA256.finalize

someFunc :: IO ()
someFunc = putStrLn $ hexHash hashTest
