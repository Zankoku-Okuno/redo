module Main where

import Control.Monad
import System.Directory
import Distribution.Redo.State as Redo


main :: IO ()
main = void $ getCurrentDirectory >>= initState