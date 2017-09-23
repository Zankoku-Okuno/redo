{-#LANGUAGE DeriveDataTypeable #-}
module Distribution.Redo.Error
    ( module Control.Exception
    , RedoException(..)
    ) where

import Control.Exception
import Type.Reflection (Typeable)


data RedoException =
      ProjectNotFound FilePath
    | TargetNotFound FilePath
    | ScriptNotFound FilePath [FilePath]
    | DoScriptFailure Int
    | DoubleOutput
    deriving (Show, Typeable)
instance Exception RedoException
