module Main where

import System.Directory
import System.FilePath
import System.Exit

import Distribution.Redo.Util
import Distribution.Redo


main :: IO ()
main = do
    whenDirectoryDoesNotExist ".redo" $ do
        createDirectory ".redo"
    whenFileDoesNotExist (".redo" </> "interpreter.conf") $ do
        writeFile (".redo" </> "interpreter.conf") "sh"
    whenFileDoesNotExist (".redo" </> "interpreter-args.conf") $ do
        writeFile (".redo" </> "interpreter-args.conf") "-xe"
    runRedo mkSkeleton =<< varsFromEnv ""


whenDirectoryDoesNotExist dir action = do
    r <- doesDirectoryExist dir
    when (not r) action

whenFileDoesNotExist f action = do
    r <- doesDirectoryExist f
    when (not r) action
