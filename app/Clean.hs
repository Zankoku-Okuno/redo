module Main where

import Distribution.Redo


main :: IO ()
main = runRedo cleanSkeleton =<< varsFromEnv ""