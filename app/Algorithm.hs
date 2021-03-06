module Algorithm
    ( isUpToDate
    , redoCheck
    ) where

import System.Process
import System.Exit

import Distribution.Redo.Util
import Distribution.Redo


isUpToDate :: TargetPath -> Redo Bool
isUpToDate target = status target >>= \case
    Changed -> do
        debug $ "--- ifchange " ++ show target
        debug "--- already rebuilt"
        return True
    NoChange -> do
        debug $ "--- ifchange " ++ show target
        debug "--- no change"
        return True
    Failure -> do
        debug $ "--- ifchange " ++ show target
        debug "--- failed"
        return False
    Unknown -> do
        debug $ "--- ifchange " ++ show target
        changeDetected <- checkForChanges target
        debug $ "--- computing: " ++ (if changeDetected then "" else "no ") ++ "change detected"
        nochange <- if changeDetected
            then return False
            else and <$> (mapM isUpToDate =<< getDeps target)
        when nochange $ recordNoChange target
        return nochange


redoCheck :: FilePath -> Redo Bool -> IO Result
redoCheck "" _ = return Skip
redoCheck pretarget isUpToDate = do
    vars <- varsFromEnv pretarget
    flip runRedo vars $ do
        target <- asks _target
        debug $ "====== " ++ show target
        do 
            depth <- asks _depth
            when (depth == 0) $ mkSkeleton >> clearStatus
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
                recordChanged target
            Skip -> do
                debug $ "=== skipping " ++ show pretarget
                recordNoChange target
            _ -> do
                debug $ "=== build failure " ++ pretarget
                recordBuildFailure target
        asks _parent >>= \case
            Nothing -> return ()
            Just parent -> parent `addDep` TargetDep target
        return result
