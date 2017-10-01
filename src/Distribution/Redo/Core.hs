module Distribution.Redo.Core
    ( module Distribution.Redo.Basic
    , RedoException(..)
    , always, ifchange

    , startup, redo, stdCatch
    , runDoScript
    ) where

import qualified Data.ByteString.Lazy as LBS
import Data.List
import Control.Monad
import Crypto.Hash.SHA1

import System.Directory
import System.Environment
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import System.Exit -- FIXME remove this when I'm done debugging this branch

import Distribution.Redo.Basic
import Distribution.Redo.Error
import Distribution.Redo.State


startup :: IO Project
startup = do
    runningFrom <- getCurrentDirectory
    project <- openProject runningFrom
    -- FIXME check if this is a child process; if so, use the project stored in the cache, otherwise create a project in the current directory
    project <- case project of
        Nothing -> throw $ ProjectNotFound runningFrom
        Just it -> pure it
    -- build@Build{..} <- getBuild project
    pure project

redo :: (Project -> Target -> IO Bool) -> Project -> FilePath -> IO Bool
redo p project request = do
    target@Target{..} <- project `getTarget` request
    exitSuccess -- TODO allow the rest of this as I refactor
    isUpToDate <- p project target -- TODO do I need to catch anything here?
    if isUpToDate
        then dontRunDoScript project target >> pure True
        else (runDoScript project target >> pure True) `catch` (\exn -> stdCatch exn >> pure False)

always :: Project -> Target -> IO Bool
always _ _ = pure False

ifchange :: Project -> Target -> IO Bool
ifchange Project{..} Target{..} = do
    deps <- state `allDependencies` canonPath
    hPutStrLn stderr $ "INFO: " ++ show deps
    if null deps -- FIXME shouldn't deps at least include the target file?
    then pure False
    else do
        upToDates <- forM deps $ \(filetype, filename) -> do
            let filepath = case filetype of
                            "SOURCE" -> srcDir </> filename
                            "TARGET" -> buildDir </> filename
                            "SCRIPT" -> scriptDir </> filename
            (time, hash) <- doesFileExist filepath >>= \case
                False -> pure (Nothing, Nothing)
                True -> do
                    time <- getModificationTime filepath
                    hash <- hashlazy <$> LBS.readFile filepath -- TODO not if the hash hasn't been requested
                    pure (Just time, Just hash)
            isChangedByTime <- checkChangedByTime state filename time
            isChangedByHash <- checkChangedByHash state filename hash
            pure . not $ isChangedByTime || isChangedByHash
        hPutStrLn stderr $ "INFO: " ++ show upToDates
        pure $ and upToDates
-- TODO ifcreate

stdCatch :: RedoException -> IO ()
-- TODO make sure all the `RedoException`s are handled
stdCatch (ProjectNotFound runningFrom) = hPutStrLn stderr $
    "USER ERROR: could not find project; perhaps you need to `redo-init` in your project directory?"
stdCatch (TargetNotFound canonPath) = hPutStrLn stderr $
    "USER ERROR: could not find target " ++ show canonPath
stdCatch (ScriptNotFound srcPath scriptPaths) = hPutStrLn stderr $ unlines
    [ "USER ERROR: could not find either source file or do-script."
    , "\tlooked for source file at " ++ show srcPath
    , "\tlooked for script files at " ++ intercalate ", " (show <$> scriptPaths)
    ]
stdCatch (DoScriptFailure exitCode) = hPutStrLn stderr $
    "USER ERROR: do-script failed with exit code " ++ show exitCode
stdCatch DoubleOutput = hPutStrLn stderr $
    "USER ERROR: do-script wrote to both tmp file and stdout"


-- TODO I probably need to mask async exceptiosn in extra places
runDoScript :: Project -> Target -> IO ()
runDoScript project@Project{..} target@Target{..} =
    let setup = do
            case scriptPath of
                Just _  -> state `register` (TargetFile, canonPath)
                Nothing -> state `register` (SourceFile, canonPath)
            state `markBuilding` canonPath
            state `clearDependencies` canonPath
            case parent of
                Just parent -> state `addIfChangeDependency` (parent, canonPath)
                Nothing    -> pure ()
            forM_ (makeRelative scriptDir <$> counterfactualScripts) $ \dependency -> do
                state `register` (ScriptFile, dependency)
                state `addIfCreateDependency` (canonPath, dependency)
            case scriptPath of
                Just (dependency, _) -> do
                    let canonDepPath = makeRelative scriptDir dependency
                    state `register` (ScriptFile, canonDepPath)
                    state `addIfChangeDependency` (canonPath, canonDepPath)
                    -- FIXME don't redo these if they're already up-to-date
                    time <- getModificationTime dependency
                    hash <- hashlazy <$> LBS.readFile dependency -- TODO not if the hash hasn't been requested
                    state `markUpToDate` (canonDepPath, Just time, Just hash)
                Nothing -> pure ()
        body = do
            runDoScript_plain project target
            time <- getModificationTime outPath
            hash <- hashlazy <$> LBS.readFile outPath -- TODO not if the hash hasn't been requested
            state `markUpToDate` (canonPath, Just time, Just hash)
        cleanAfterError = state `markBuildFailed` canonPath
        cleanup = case parent of
            Just _ -> pure ()
            Nothing -> finishBuild state
    in setup >> (body `onException` cleanAfterError) `finally` cleanup

dontRunDoScript :: Project -> Target -> IO ()
dontRunDoScript Project{..} Target{..} = case parent of
    Just parent -> state `addIfChangeDependency` (parent, canonPath)
    Nothing -> finishBuild state

runDoScript_plain :: Project -> Target -> IO ()
runDoScript_plain Project{..} Target{..} = case scriptPath of
    Just (scriptPath, (baseName, _)) -> do
        ensureDestination -- NOTE just in case anything in the script automatically puts generated files in cwd
        withOutputs $ \tmpPath (pipePath, pipeHandle) -> do
            let moreEnv = [("REDO__TARGET", canonPath)] -- TODO do I need any more env vars?
            oldEnv <- getEnvironment
            let newEnv = oldEnv ++ moreEnv -- FIXME I thought the other way around would work; I need to manipulate the env like an env instead of an alist
                args = [outPath, baseName, tmpPath] -- TODO implement an option for new-style arguments to the script
                cmd = case doScriptLang of
                    Nothing -> proc scriptPath args
                    Just sh -> shell $ sh ++ (' ':scriptPath) ++ concatMap (' ':) args
                process = cmd {
                      cwd = Just outDir
                    , env = Just newEnv
                    , std_in = Inherit
                    , std_out = UseHandle pipeHandle
                    , std_err = Inherit
                }
            withCreateProcess process $ \procIn procOut procErr procHandle -> do
                exitCode <- waitForProcess procHandle
                case exitCode of
                    ExitSuccess -> commitOutput (tmpPath, pipePath)
                    ExitFailure code -> throw $ DoScriptFailure code
    Nothing -> do
        when (outPath /= srcPath) $ do
            ensureDestination
            copyFile srcPath outPath
    where
    ensureDestination = createDirectoryIfMissing True outDir
    outDir = takeDirectory outPath
    -- TODO could I just use `/dev/fd/1`?
    withOutputs :: (FilePath -> (FilePath, Handle) -> IO a) -> IO a
    withOutputs action =
        withSystemTempFile (takeFileName outPath) $ \tmpPath closeThisHandle ->
        withSystemTempFile (takeFileName outPath ++ "-pipe") $ \pipePath pipeHandle -> do
            hClose closeThisHandle
            action tmpPath (pipePath, pipeHandle)
    commitOutput :: (FilePath, FilePath) -> IO ()
    commitOutput (outfile, outpipe) = do
        fileExists <- doesFileExist outfile
        fromFile <- if fileExists then withFile outfile ReadMode hFileSize else pure 0
        fromStdout <- withFile outpipe ReadMode hFileSize
        case (fileExists, fromFile, fromStdout) of
            (False, _, 0) -> pure () -- file was removed and no stdout was written to
            (False, _, _) -> throw DoubleOutput -- file was removed, but stdout was written to
             -- TODO copyFile should be renameFile, if that won't mess `withSystemTempFile`, and if I can ensure I won't run into the caveats
            (True, _, 0) -> copyFile outfile outPath -- stdout was not written to, file exists but we don't care what's in it
            (True, 0, _) -> copyFile outpipe outPath -- stdout was written to instead of the file
            (True, _, _) -> throw DoubleOutput -- both the file and stdout was written to