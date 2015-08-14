{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Process
import System.Directory
import System.FilePath
import System.Exit

import Distribution.Redo.Util
import Distribution.Redo.Monad
import Distribution.Redo.Concepts

-- FIXME eliminate magic strings

-- TODO respond to dependencies
-- TODO research multi-process threading
-- TODO log what is happening



main :: IO ()
main = do
    cmd <- fromMaybe "always" . stripPrefix "redo-" <$> getProgName
    case cmd of
        --TODO redo-init
        --TODO redo-clean
        "always" -> do
            results <- mapM (`redoCheck` redoAlways) =<< getArgs
            when (or $ isBadResult <$> results) exitFailure
        "ifchange" -> do
            results <- mapM (`redoCheck` redoIfChange) =<< getArgs
            when (or $ isBadResult <$> results) exitFailure
        _ -> die $ "unrecognized redo command (" ++ cmd ++ ")"

isBadResult :: Result -> Bool
isBadResult (Bad _) = True
isBadResult Skip = False
isBadResult (Run ExitSuccess) = False
isBadResult (Run (ExitFailure _)) = True

redoAlways :: Redo Bool
redoAlways = do
    debug "--- always"
    return False

redoIfChange :: Redo Bool
redoIfChange = isUpToDate =<< asks _target

isUpToDate :: TargetPath -> Redo Bool
isUpToDate target = do
    utd <- liftIO . doesFileExist =<< mkUTDFile target
    failed <- liftIO . doesFileExist =<< mkFailFile target
    result <- case (utd, failed) of
        (True, _) -> do
                        debug $ "--- ifchange " ++ show target
                        debug "--- utd file found"
                        return True
        (_, True) -> do
                        debug $ "--- ifchange " ++ show target
                        debug "--- fail file found"
                        return False
        _ -> do
            changeDetected <- checkForChanges target
            debug $ "--- ifchange " ++ show target
            debug $ "--- change detected? " ++ show changeDetected
            case changeDetected of
                True -> return False
                False -> and <$> (mapM isUpToDate =<< getDeps target)
    when result $ recordUpToDate target
    return result


redoCheck :: FilePath -> Redo Bool -> IO Result
redoCheck "" _ = return Skip
redoCheck pretarget isUpToDate = do
    vars <- mkVars pretarget
    flip runRedo vars $ do
        target <- asks _target
        debug $ "====== " ++ show target
        do 
            depth <- asks _depth
            when (depth == 0) mkSkeleton
        result <- isUpToDate >>= \case
            False -> do
                debug "=== not up-to-date"
                clearDeps target
                scripts <- redoScripts
                (target `addDep`) `mapM` (ScriptDep <$> _missing scripts)
                case _found scripts of
                    Nothing -> do
                        debug "=== source file"
                        doSourceFile
                    Just script -> do
                        target `addDep` ScriptDep script
                        debug "=== running script"
                        Run <$> doTargetFile script (_targetBasePath scripts)
            True -> do
                debug "=== up-to-date"
                return Skip
        case result of
            Run ExitSuccess -> do
                debug $ "=== updated " ++ show pretarget
                recordChange target
                recordUpToDate target
            Skip -> do
                debug $ "=== skipping " ++ show pretarget
                recordUpToDate target
            _ -> do
                debug $ "=== build failure " ++ pretarget
                recordBuildFailure target
        asks _parent >>= \case
            Nothing -> return ()
            Just parent -> parent `addDep` TargetDep target
        return result

doSourceFile :: Redo Result
doSourceFile = sourceIsValid >>= \case
    True -> return Skip
    False -> return . Bad $ "cannot find either source file or build script"

doTargetFile :: ScriptPath -> Maybe TargetBasePath -> Redo ExitCode
doTargetFile script targetBasePath = withTmpFile $ \tmpfile tmpfp -> do
    env' <- mkScriptEnv
    userargs <- asks _shArgs
    let redoargs = mkScriptArgs script targetBasePath tmpfile
    let args = userargs ++ redoargs
    cwd' <- mkScriptCwd
    interpreter <- asks _sh
    let cmd = proc interpreter args
        process = cmd { std_out = UseHandle tmpfp
                      , env = Just env'
                      , cwd = Just cwd'
                      }
    -- Run Process
    (_, _, _, ph) <- liftIO $ createProcess process
    liftIO $ waitForProcess ph

