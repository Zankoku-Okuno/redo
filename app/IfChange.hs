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
    results <- mapM (`redoCheck` redoIfChange) =<< getArgs
    when (or $ isBadResult <$> results) exitFailure

redoIfChange :: Redo Bool
redoIfChange = isUpToDate =<< asks _target