module Algorithm where

import System.Process
import System.Exit

import Distribution.Redo.Util
import Distribution.Redo

-- FIXME eliminate magic strings
-- FIXME if, after redoing the dependencies, none of them actually changed, then we can (reverse-?)prune the target from being built
-- FIXME I'm recomputing the hash every time

-- TODO log what is happening
-- TODO research multi-process threading



isUpToDate :: TargetPath -> Redo Bool
isUpToDate target = status target >>= \case
    UpToDate -> do
        debug $ "--- ifchange " ++ show target
        debug "--- already built"
        return True
    Failed -> do
        debug $ "--- ifchange " ++ show target
        debug "--- already failed"
        return False
    Uncomputed -> do
        debug $ "--- ifchange " ++ show target
        changeDetected <- checkForChanges target
        debug $ "--- computing: " ++ show changeDetected
        result <- case changeDetected of
            True -> return False
            False -> and <$> (mapM isUpToDate =<< getDeps target)
        when result $ recordUnchanged target
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
            when (depth == 0) clearStatus
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
            Skip -> do
                debug $ "=== skipping " ++ show pretarget
                recordChange target
            _ -> do
                debug $ "=== build failure " ++ pretarget
                recordBuildFailure target
        asks _parent >>= \case
            Nothing -> return ()
            Just parent -> parent `addDep` TargetDep target
        return result




