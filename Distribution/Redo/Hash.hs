module Distribution.Redo.Hash (
      Hash
    , hash16, hash16lazy, hash16utf8
    , readHashFile, writeHashFile
    , hashContents
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Distribution.Redo.Util

import System.Directory (doesFileExist)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256


newtype Hash = Hash BS.ByteString
    deriving(Eq)
instance Show Hash where
    show (Hash hash) = UTF8.toString hash
--TODO Read instance, using Base16.decode

hash16 :: BS.ByteString -> Hash
hash16 = Hash . Base16.encode . SHA256.hash

hash16lazy :: BL.ByteString -> Hash
hash16lazy = Hash . Base16.encode . SHA256.hashlazy

hash16utf8 :: String -> Hash
hash16utf8 = hash16 . UTF8.fromString

readHashFile :: FilePath -> IO Hash
readHashFile hashFile = Hash <$> BS.readFile hashFile

writeHashFile :: FilePath -> Hash -> IO ()
writeHashFile hashFile (Hash hash) = BS.writeFile hashFile hash

hashContents :: FilePath -> IO Hash
hashContents filepath = do
    exists <- doesFileExist filepath
    if exists
        then hash16lazy <$> BL.readFile filepath
        else return $ hash16utf8 ""
