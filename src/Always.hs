module Main (main) where

import Control.Exception
import Control.Monad
import System.Environment
import System.Exit

import Distribution.Redo.Core


main :: IO ()
main = do
    requests <- getArgs -- TODO use some kind of argparse
    project <- startup `catch` (\exn -> stdCatch exn >> exitFailure)
    allAreGood <- and <$> forM requests (redo always project)
    if allAreGood then exitSuccess else exitFailure

