module Distribution.Redo
    ( -- * Redo Monad
      Redo, runRedo
    , debug
    
    , Status(..), status
    , DepPath(..)
    , getDeps, addDep, clearDeps
    , checkForChanges
    , recordChange, recordUpToDate, recordBuildFailure

    -- * Environment
    , Vars(..), varsFromEnv
    -- ** Paths
    , Scripts(..), findScript, ScriptPath, TargetBasePath
    , ProjDir
    , TargetPath, doesTargetExist

    , workerProcess
    , mkSkeleton, cleanSkeleton

    , Result(..), isBadResult
    ) where

import Distribution.Redo.Util
import Distribution.Redo.Env
import Distribution.Redo.Monad

import System.Directory
import System.FilePath
import System.Exit


mkSkeleton :: Redo ()
mkSkeleton = do
    (ProjDir projDir) <- asks _projDir
    liftIO $ do
        createDirectoryIfMissing False $ projDir </> "deps"
        createDirectoryIfMissing False $ projDir </> "hashes"
        do
            exists <- doesDirectoryExist $ projDir </> "utd"
            when exists $ removeDirectoryRecursive $ projDir </> "utd"
            createDirectoryIfMissing False $ projDir </> "utd"
        createDirectoryIfMissing False $ projDir </> "fails"

cleanSkeleton :: Redo ()
cleanSkeleton = do
    (ProjDir projDir) <- asks _projDir
    liftIO $ do
        depsExists <- doesDirectoryExist $ projDir </> "deps"
        when depsExists $ removeDirectoryRecursive $ projDir </> "deps"
        hashesExists <- doesDirectoryExist $ projDir </> "hashes"
        when hashesExists $ removeDirectoryRecursive $ projDir </> "hashes"
        utdExists <- doesDirectoryExist $ projDir </> "utd"
        when utdExists $ removeDirectoryRecursive $ projDir </> "utd"
        failsExists <- doesDirectoryExist $ projDir </> "fails"
        when failsExists $ removeDirectoryRecursive $ projDir </> "fails"


data Result = Skip | Run ExitCode | Bad String

isBadResult :: Result -> Bool
isBadResult (Bad _) = True
isBadResult Skip = False
isBadResult (Run ExitSuccess) = False
isBadResult (Run (ExitFailure _)) = True
