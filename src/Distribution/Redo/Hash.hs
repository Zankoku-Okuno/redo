module Distribution.Redo.Hash (
      Hash
    , hash16, hash16lazy, hash16utf8
    , readHashFile, writeHashFile
    , hashContents
    , readHash, showHash
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Distribution.Redo.Util

import System.Directory (doesFileExist)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8 -- TODO remove this dependency
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256


newtype Hash = Hash BS.ByteString
    deriving(Eq)


showHash :: Hash -> String
showHash (Hash hash) = T.unpack . T.decodeUtf8 $ Base16.encode hash
readHash :: String -> Hash
readHash str = Hash . fst . Base16.decode . T.encodeUtf8 . T.pack $ str

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

hashContents :: FilePath -> IO (Maybe Hash)
hashContents filepath = do
    exists <- doesFileExist filepath
    if exists
        then Just . hash16lazy <$> BL.readFile filepath
        else return $ Nothing
