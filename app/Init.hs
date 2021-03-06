module Main
    ( main
    ) where

import System.Directory
import System.FilePath
import System.Exit

import Distribution.Redo.Util
import Distribution.Redo


main :: IO ()
main = do
    whenDirectoryDoesNotExist ".redo" $
        createDirectory ".redo"
    whenFileDoesNotExist (".redo" </> "interpreter.conf") $
        writeFile (".redo" </> "interpreter.conf") "sh"
    whenFileDoesNotExist (".redo" </> "interpreter-args.conf") $
        writeFile (".redo" </> "interpreter-args.conf") "-xe"
    runRedo mkSkeleton =<< varsFromEnv ""


whenDirectoryDoesNotExist dir action = do
    r <- doesDirectoryExist dir
    unless r action

whenFileDoesNotExist f action = do
    r <- doesDirectoryExist f
    unless r action
