module Distribution.Redo.Hash (
      Hash
    , hash16, hash16lazy
    , hashContents
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Distribution.Redo.Util

import System.Directory (doesFileExist)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256

import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField


newtype Hash = Hash BS.ByteString
    deriving(Eq)
instance Show Hash where
    show = showHash
instance ToField Hash where
    toField (Hash hash) = toField  hash
instance FromField Hash where
    fromField = (Hash <$>) . fromField

showHash :: Hash -> String
showHash (Hash hash) = T.unpack . T.decodeUtf8 $ Base16.encode hash
readHash :: String -> Hash
readHash str = Hash . fst . Base16.decode . T.encodeUtf8 . T.pack $ str

hash16 :: BS.ByteString -> Hash
hash16 = Hash . Base16.encode . SHA256.hash

hash16lazy :: BL.ByteString -> Hash
hash16lazy = Hash . Base16.encode . SHA256.hashlazy

hashContents :: FilePath -> IO Hash
hashContents filepath = do
    exists <- doesFileExist filepath
    if exists
        then hash16lazy <$> BL.readFile filepath
        else return $ Hash ""
