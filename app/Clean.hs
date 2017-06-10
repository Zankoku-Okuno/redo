module Main
    ( main
    ) where

import Distribution.Redo


main :: IO ()
main = runRedo cleanSkeleton =<< varsFromEnv ""