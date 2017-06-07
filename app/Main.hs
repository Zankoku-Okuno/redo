{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Process
import System.Directory
import System.FilePath
import System.Exit

import Distribution.Redo.Util
import Distribution.Redo

-- FIXME eliminate magic strings
-- FIXME if a file was empty and then deleted, that isn't counting as a change
-- FIXME if, after redoing the dependencies, none of them actually changed, then we can (reverse-?)prune the target from being built

-- TODO log what is happening
-- TODO research multi-process threading



main :: IO ()
main = do
    cmd <- fromMaybe "always" . stripPrefix "redo-" <$> getProgName
    case cmd of
        "init" -> do
            createDirectory ".redo"
            writeFile ".redo/interpreter.conf" "sh"
            writeFile ".redo/interpreter-args.conf" "-xe"
            runRedo mkSkeleton =<< varsFromEnv ""
        "clean" -> runRedo cleanSkeleton =<< varsFromEnv ""
        "always" -> do
            results <- mapM (`redoCheck` redoAlways) =<< getArgs
            when (or $ isBadResult <$> results) exitFailure
        "ifchange" -> do
            results <- mapM (`redoCheck` redoIfChange) =<< getArgs
            when (or $ isBadResult <$> results) exitFailure
        _ -> die $ "unrecognized redo command (" ++ cmd ++ ")"

redoAlways :: Redo Bool
redoAlways = do
    debug "--- always"
    return False

redoIfChange :: Redo Bool
redoIfChange = isUpToDate =<< asks _target

isUpToDate :: TargetPath -> Redo Bool
isUpToDate target = status target >>= \case
    UpToDate -> do
        debug $ "--- ifchange " ++ show target
        debug "--- utd file found"
        return True
    Failed -> do
        debug $ "--- ifchange " ++ show target
        debug "--- fail file found"
        return False
    Uncomputed -> do
        changeDetected <- checkForChanges target
        debug $ "--- ifchange " ++ show target
        debug $ "--- change detected? " ++ show changeDetected
        result <- case changeDetected of
            True -> return False
            False -> and <$> (mapM isUpToDate =<< getDeps target)
        when result $ recordUpToDate target
        return result


redoCheck :: FilePath -> Redo Bool -> IO Result
redoCheck "" _ = return Skip
redoCheck pretarget isUpToDate = do
    vars <- varsFromEnv pretarget
    flip runRedo vars $ do
        target <- asks _target
        debug $ "====== " ++ show target
        do 
            depth <- asks _depth
            when (depth == 0) mkSkeleton
        result <- isUpToDate >>= \case
            True -> do
                debug "=== up-to-date"
                return Skip
            False -> do
                debug "=== not up-to-date"
                clearDeps target
                scripts <- liftIO $ findScript target
                (target `addDep`) `mapM` (ScriptDep <$> _missing scripts)
                case _found scripts of
                    Nothing -> do
                        debug "=== source file"
                        liftIO $ doesTargetExist target >>= \case
                            True -> return Skip
                            False -> return . Bad $ "cannot find either source file or build script"
                    Just script -> do
                        target `addDep` ScriptDep script
                        debug "=== running script"
                        vars <- ask
                        (Run <$>) $ workerProcess vars scripts $ \process -> liftIO $ do
                            (_, _, _, ph) <- createProcess process
                            waitForProcess ph
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




