module Main where

import System.Directory
import System.FilePath

import Distribution.Redo


main :: IO ()
main = do
    createDirectory ".redo"
    writeFile (".redo" </> "interpreter.conf") "sh"
    writeFile (".redo" </> "interpreter-args.conf") "-xe"
    runRedo mkSkeleton =<< varsFromEnv ""
