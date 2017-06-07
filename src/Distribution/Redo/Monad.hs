{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Distribution.Redo.Monad (
      Redo, runRedo

    , Status(..), status
    , DepPath(..)
    , getDeps, addDep, clearDeps
    , checkForChanges
    , recordChange, recordUpToDate, recordBuildFailure

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



newtype Redo a = Redo { unRedo :: ReaderT Vars IO a }
    deriving(Functor, Applicative, Monad, MonadIO)

instance MonadReader Vars Redo where
    ask = Redo ask
    local f action = Redo $ local f (unRedo action)

runRedo :: Redo a -> Vars -> IO a
runRedo action vars = flip runReaderT vars $ unRedo action




data Status = UpToDate | Failed | Uncomputed

status :: TargetPath -> Redo Status
status (TargetPath targetPath) = do
    utd <- liftIO . doesFileExist =<< mkUTDFile targetPath
    failed <- liftIO . doesFileExist =<< mkFailFile targetPath
    return $ case (utd, failed) of
        (_, True) -> Failed
        (True, _) -> UpToDate
        _ -> Uncomputed


data DepPath = TargetDep TargetPath
             | ScriptDep ScriptPath

getDeps :: TargetPath -> Redo [TargetPath]
getDeps (TargetPath target) = do
    depsFile <- mkDepsFile target
    exists <- liftIO $ doesFileExist depsFile
    liftIO $ if exists then map TargetPath . lines <$> readFile depsFile else return []

addDep :: TargetPath -> DepPath -> Redo ()
addDep (TargetPath parent) (ScriptDep (ScriptPath child)) = do
    depsFile <- mkDepsFile parent
    liftIO $ withFile depsFile AppendMode $ flip hPutStrLn child
    hashFile <- mkHashFile child
    scriptHash <- liftIO $ hashContents child
    liftIO $ writeHashFile hashFile scriptHash
addDep (TargetPath parent) (TargetDep (TargetPath child)) = do
    depsFile <- mkDepsFile parent
    liftIO $ withFile depsFile AppendMode $ flip hPutStrLn child

clearDeps :: TargetPath -> Redo ()
clearDeps (TargetPath target) = do
    depsFile <- mkDepsFile target
    liftIO $ removeFileIfExists depsFile

checkForChanges :: TargetPath -> Redo Bool
checkForChanges (TargetPath target) = do
    hashFile <- mkHashFile target
    hashExists <- liftIO $ doesFileExist hashFile
    if not hashExists then return True else do
        lastHash <- liftIO $ readHashFile hashFile
        currentHash <- liftIO $ hashContents target
        --debug $ "last hash: " ++ show lastHash
        --debug $ "curr hash: " ++ show currentHash
        return $ currentHash /= lastHash

recordChange :: TargetPath -> Redo ()
recordChange (TargetPath target) = do
    hashFile <- mkHashFile target
    liftIO $ writeHashFile hashFile =<< hashContents target

recordUpToDate :: TargetPath -> Redo ()
recordUpToDate (TargetPath target) = do
    liftIO . removeFileIfExists =<< mkFailFile target
    exists <- liftIO $ doesFileExist target
    when exists $ do
        liftIO . flip writeFile "" =<< mkUTDFile target

recordBuildFailure :: TargetPath -> Redo ()
recordBuildFailure (TargetPath target) = do
    liftIO . removeFileIfExists =<< mkUTDFile target
    liftIO . flip writeFile "" =<< mkFailFile target


--FIXME use proper logging libraries
debug :: String -> Redo ()
debug msg = do
    depth <- asks _depth
    let indentedMsg = concat (replicate depth "  ") ++ msg
    liftIO $ putErrLn indentedMsg





mkHashFile :: FilePath -> Redo FilePath
mkHashFile = mkFooFile "hashes"
mkDepsFile :: FilePath -> Redo FilePath
mkDepsFile = mkFooFile "deps"
mkUTDFile :: FilePath -> Redo FilePath
mkUTDFile = mkFooFile "utd"
mkFailFile :: FilePath -> Redo FilePath
mkFailFile = mkFooFile "fails"

mkFooFile :: String -> FilePath -> Redo FilePath
mkFooFile foo path = do
    let hash = hash16utf8 path
    (ProjDir projDir) <- asks _projDir
    return $ projDir </> foo </> show hash
