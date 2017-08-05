{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , Hash
    , genesisHash
    , nextHash
    ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Text.Printf (printf)
import Data.DateTime (DateTime, getCurrentTime, toSqlString)

data Hash = Hash { index :: Int
                 , timestamp :: DateTime
                 , content :: String -- "data" in Snakecoin
                 , previousHash :: String
                 , hash :: String
                 } deriving (Show)

hashBlock :: Hash -> String
hashBlock hsh = hh where h0 = SHA256.init
                         cntnt = mconcat [ show $ index hsh
                                         , toSqlString $ timestamp hsh
                                         , content hsh
                                         , previousHash hsh
                                         ]
                         packedContent = C.pack cntnt
                         h1 = SHA256.update h0 packedContent
                         hh = hexHash h1

makeHash :: Int -> String -> String -> IO Hash
makeHash n cntnt prvsHsh = do
  now <- getCurrentTime
  let preHash = Hash { index = n
                     , timestamp = now
                     , content = cntnt
                     , previousHash = prvsHsh
                     , hash = "" -- KLUDGE: will be overwritten
                     }
  return preHash { hash = hashBlock preHash }

genesisHash :: IO Hash
genesisHash = makeHash 0 "Genesis Block" "0"

nextHash :: String -> Hash -> IO Hash
nextHash cntnt hsh = makeHash newIndex newContent (previousHash hsh) where
    newIndex = 1 + index hsh
    newContent = if null cntnt
                 then "Hey! I'm block " ++ show newIndex
                 else cntnt

hash1 :: IO Hash
hash1 = nextHash "" =<< genesisHash

hexHash :: SHA256.Ctx -> String
hexHash = concatMap (printf "%02x") . B.unpack . SHA256.finalize

someFunc :: IO ()
someFunc = (show <$> hash1) >>= putStrLn
