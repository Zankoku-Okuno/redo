module Distribution.Redo
    ( -- * Redo Monad
      Redo, runRedo
    , debug
    
    , Status(..), status
    , DepPath(..)
    , getDeps, addDep, clearDeps
    , checkForChanges
    , clearStatus, recordChanged, recordNoChange, recordBuildFailure

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



data Result = Skip | Run ExitCode | Bad String

isBadResult :: Result -> Bool
isBadResult (Bad _) = True
isBadResult Skip = False
isBadResult (Run ExitSuccess) = False
isBadResult (Run (ExitFailure _)) = True
