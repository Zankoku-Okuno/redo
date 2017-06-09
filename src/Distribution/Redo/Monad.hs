{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Distribution.Redo.Monad (
      Redo, runRedo

    , Status(..), status
    , DepPath(..)
    , getDeps, addDep, clearDeps
    , checkForChanges
    , clearStatus, recordChange, recordUnchanged, recordBuildFailure

    , mkSkeleton, cleanSkeleton
    , debug
    ) where

import Distribution.Redo.Util
import Distribution.Redo.Env
import Distribution.Redo.Hash

import Control.Monad.Trans
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import System.IO
import System.Directory
import System.FilePath
import qualified Database.SQLite.Simple as Sql


newtype Redo a = Redo { unRedo :: ReaderT Vars IO a }
    deriving(Functor, Applicative, Monad, MonadIO)

instance MonadReader Vars Redo where
    ask = Redo ask
    local f action = Redo $ local f (unRedo action)

runRedo :: Redo a -> Vars -> IO a
runRedo action vars = flip runReaderT vars $ unRedo action




data Status = UpToDate | Failed | Uncomputed
    deriving(Eq, Show, Read)



data DepPath = TargetDep TargetPath
             | ScriptDep ScriptPath

getDeps :: TargetPath -> Redo [TargetPath]
getDeps target = withDb $ \db -> do
    rows <- Sql.query db "SELECT dependency FROM deps WHERE target = ?;"
                         (Sql.Only target)
    pure $ Sql.fromOnly <$> rows

addDep :: TargetPath -> DepPath -> Redo ()
addDep target (ScriptDep (ScriptPath script)) = do
    scriptHash <- liftIO $ hashContents script
    withDb $ \db -> do
        Sql.execute db "INSERT OR REPLACE INTO targets (target, status, hash) VALUES (?, ?, ?)"
                       (script, show UpToDate, scriptHash)
        Sql.execute db "INSERT INTO deps (target, dependency) VALUES (?, ?);"
                       (target, script)
addDep target (TargetDep dependency) = withDb $ \db -> do
    Sql.execute db "INSERT INTO deps (target, dependency) VALUES (?, ?);"
                   (target, dependency)

clearDeps :: TargetPath -> Redo ()
clearDeps target = withDb $ \db -> do
    Sql.execute db "DELETE FROM deps WHERE target = ?"
                   (Sql.Only target)

checkForChanges :: TargetPath -> Redo Bool
checkForChanges target = do
    rows <- withDb $ \db -> Sql.query db "SELECT hash FROM targets WHERE target = ?;"
                         (Sql.Only target)
    let lastHash = case rows of
                    [(Sql.Only hash)] -> Just $ hash
                    [] -> Nothing
                    _ -> error "sql error reading hash"
    currentHash <- liftIO $ Just <$> hashContents (targetAsFilePath target)
    -- debug $ "last hash: " ++ show (showHash <$> lastHash)
    -- debug $ "current hash: " ++ show (showHash <$> currentHash)
    return $ currentHash /= lastHash


status :: TargetPath -> Redo Status
status target = withDb $ \db -> do
    rows <- Sql.query db "SELECT status FROM targets WHERE target = ?"
                         (Sql.Only target)
    pure $ case rows of
        [(Sql.Only status)] -> read status
        [] -> Uncomputed
        _ -> error "sql problem obtaining status"

clearStatus :: Redo ()
clearStatus = withDb $ \db -> do
    Sql.execute_ db "UPDATE targets SET status = 'Uncomputed';"

recordChange :: TargetPath -> Redo ()
recordChange target = withDb $ \db -> do
    hash <- liftIO $ hashContents (targetAsFilePath target)
    Sql.execute db "INSERT OR REPLACE INTO targets (target, status, hash) VALUES (?, ?, ?);"
                   (target, show UpToDate, hash)

recordUnchanged :: TargetPath -> Redo ()
recordUnchanged target = withDb $ \db -> do
    Sql.execute db "INSERT OR REPLACE INTO targets (target, status, hash) VALUES (?, ?, (SELECT hash FROM targets WHERE target = ?));"
                   (target, show UpToDate, target)

recordBuildFailure :: TargetPath -> Redo ()
recordBuildFailure target = withDb $ \db -> do
    Sql.execute db "INSERT OR REPLACE INTO targets (target, status, hash) VALUES (?, ?, (SELECT hash FROM targets WHERE target = ?));"
                   (target, show Failed, target)


debug :: String -> Redo ()
debug msg = do
    depth <- asks _depth
    let indentedMsg = concat (replicate depth "  ") ++ msg
    liftIO $ putErrLn indentedMsg








mkSkeleton :: Redo ()
mkSkeleton = do
    withDb $ \db -> do
        Sql.execute_ db "CREATE TABLE IF NOT EXISTS targets (target TEXT NOT NULL PRIMARY KEY, status TEXT NOT NULL, hash BLOB NOT NULL);"
        Sql.execute_ db "CREATE TABLE IF NOT EXISTS deps (target TEXT NOT NULL, dependency TEXT NOT NULL);"

cleanSkeleton :: Redo ()
cleanSkeleton = withDb $ \db -> do
    Sql.execute_ db "DELETE FROM targets;"
    Sql.execute_ db "DELETE FROM deps;"


withDb :: (Sql.Connection -> IO a) -> Redo a
withDb action = do
    (ProjDir projDir) <- asks _projDir
    liftIO $ Sql.withConnection (projDir </> "redo.sqlite3") action