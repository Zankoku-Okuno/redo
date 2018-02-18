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

getSandbox :: FilePath -> IO (Maybe Sandbox)
getSandbox filepath = do
    startDir <- makeAbsolute $ dropTrailingPathSeparator filepath
    doesPathExist startDir >>= \case
        False -> pure Nothing
        True -> do
            let candidates = (</> ".redo") <$> takeWhile notRedoDir (iterateUp startDir)
            redoDirs <- filterM doesDirectoryExist candidates
            pure $ case redoDirs of
                [] -> Nothing
                (_redoDir:_) -> Just $ Sandbox {..}
    where
    iterateUp dir0 = let dir' = takeDirectory dir0 in if dir' == dir0 then [dir0] else dir0 : iterateUp dir'
    notRedoDir path = takeFileName path /= ".redo"
