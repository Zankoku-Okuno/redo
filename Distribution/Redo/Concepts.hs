{-# LANGUAGE LambdaCase #-}
module Distribution.Redo.Concepts (
      mkVars

    , sourceIsValid
    , redoScripts

    , debug
    ) where

import Distribution.Redo.Util
import Distribution.Redo.Hash
import Distribution.Redo.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import System.IO
import System.Environment
import System.Directory
import System.FilePath


mkVars :: FilePath -> IO Vars
mkVars filepath = do
    target <- targetPath filepath
    parent <- parentPath
    projDir <- lookupProjDir >>= \case
        Nothing -> die "cannot find redo project"
        Just it -> return it
    depth <- redoDepth
    (sh, shArgs) <- interpreter projDir
    -- package it up
    return Vars {
          _target = target
        , _projDir = projDir
        , _sh = sh
        , _shArgs = shArgs
        , _depth = depth
        , _parent = parent
    }


sourceIsValid :: Redo Bool
sourceIsValid = liftIO . doesTargetExist =<< asks _target


redoScripts :: Redo Scripts
redoScripts = liftIO . findScript =<< asks _target





debug :: String -> Redo ()
debug msg = do
    depth <- asks _depth
    let indentedMsg = concat (replicate depth "    ") ++ msg
    liftIO $ putErrLn indentedMsg