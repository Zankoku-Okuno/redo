{-
    Find projects; parse & store project configuration.
-}
module Distribution.Redo.Sandbox where

import Control.Monad

import System.FilePath
import System.Directory


data Sandbox = Sandbox
    { _redoDir :: FilePath
    }

getSandbox :: FilePath -> IO Sandbox
getSandbox filepath = do
    startDir <- makeAbsolute $ dropTrailingPathSeparator filepath
    let candidates = (</> ".redo") <$> iterateUp startDir
    redoDirs <- filterM doesDirectoryExist candidates
    let _redoDir = head redoDirs -- FIXME
    pure $ Sandbox {..}
    where
    iterateUp dir0 = let dir' = takeDirectory dir0 in if dir' == dir0 then [dir0] else dir0 : iterateUp dir'
