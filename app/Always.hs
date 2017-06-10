module Main
    ( main
    ) where

import System.Environment
import System.Exit

import Distribution.Redo.Util
import Distribution.Redo
import Algorithm


main :: IO ()
main = do
    results <- mapM (`redoCheck` redoAlways) =<< getArgs
    when (or $ isBadResult <$> results) exitFailure

redoAlways :: Redo Bool
redoAlways = do
    debug "--- always"
    return False
