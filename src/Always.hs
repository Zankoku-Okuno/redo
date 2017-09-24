module Main where

import Data.Maybe
import Data.List
import Control.Monad

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

import Distribution.Redo.Basic
import Distribution.Redo.Error
import Distribution.Redo.State

main :: IO ()
main = do
    [request] <- getArgs -- TODO use some kind of argparse
    runningFrom <- getCurrentDirectory

    -- FIXME catch RedoExceptions from here
    project@Project{..} <- openProject runningFrom >>= \case
        Nothing -> throw $ ProjectNotFound runningFrom
        Just it -> pure it
    -- build@Build{..} <- getBuild project
    target@Target{..} <- project `getTarget` request
    
    hPutStrLn stderr $ "INFO: requested `" ++ show request ++ "`"
    hPutStrLn stderr $ "INFO: project is " ++ show project
    -- hPutStrLn stderr $ "INFO: build is " ++ show build
    hPutStrLn stderr $ "INFO: target is " ++ show target

    runDoScriptWithState project target
        `catch` \(exn :: RedoException) -> do
            case exn of
                -- TODO make sure all the `RedoException`s are handled
                ProjectNotFound runningFrom -> putErrLn $ "USER ERROR: could not find project; perhaps you need to `redo-init` in your project directory?"
                TargetNotFound canonPath -> putErrLn $ "USER ERROR: could not find target " ++ show canonPath
                ScriptNotFound srcPath scriptPaths -> putErrLn $ unlines [
                      "USER ERROR: could not find either source file or do-script."
                    , "\tlooked for source file at " ++ show srcPath
                    , "\tlooked for script files at " ++ intercalate ", " (show <$> scriptPaths)
                    ]
                DoScriptFailure exitCode -> putErrLn $ "USER ERROR: do-script failed with exit code " ++ show exitCode
                DoubleOutput -> putErrLn $ "USER ERROR: do-script wrote to both tmp file and stdout"
            exitFailure

putErrLn = hPutStrLn stderr

-- TODO I probably need to mask async exceptiosn in extra places
runDoScriptWithState :: Project -> Target -> IO ()
runDoScriptWithState project@Project{..} target@Target{..} =
    let setup = do
            case scriptPath of
                Just _  -> state `registerTarget` canonPath
                Nothing -> state `registerSource` canonPath
            state `markBuilding` canonPath
            state `clearDependencies` canonPath
            case child of
                Just child -> state `addIfChangeDependency` (child, canonPath)
                Nothing    -> pure ()
            forM_ (makeRelative scriptDir <$> counterfactualScripts) $ \dependency -> do
                state `registerScript` dependency
                state `addIfCreateDependency` (canonPath, dependency)
        body = do
            runDoScript project target
            state `markUpToDate` canonPath
        cleanAfterError = state `markBuildFailed` canonPath
        cleanup = case child of
            Just _ -> pure ()
            Nothing -> finishBuild state
    in setup >> (body `onException` cleanAfterError) `finally` cleanup

runDoScript :: Project -> Target -> IO ()
runDoScript Project{..} Target{..} = case scriptPath of
    Just (scriptPath, (baseName, _)) -> do
        ensureDestination -- NOTE just in case anything in the script automatically puts generated files in cwd
        withOutputs outPath $ \tmpPath (pipePath, pipeHandle) -> do
            let moreEnv = [("REDO__TARGET", canonPath)] -- TODO do I need any more env vars?
            oldEnv <- getEnvironment
            let newEnv = moreEnv ++ oldEnv
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
                    ExitSuccess -> (tmpPath, pipePath) `commitOutput` outPath
                    ExitFailure code -> throw $ DoScriptFailure code
    Nothing -> do
        when (outPath /= srcPath) $ do
            ensureDestination
            copyFile srcPath outPath
    where
    ensureDestination = createDirectoryIfMissing True outDir
    outDir = takeDirectory outPath

withOutputs :: FilePath -> (FilePath -> (FilePath, Handle) -> IO a) -> IO a
withOutputs outPath action =
    withSystemTempFile (takeFileName outPath) $ \tmpPath closeThisHandle ->
    withSystemTempFile (takeFileName outPath ++ "-pipe") $ \pipePath pipeHandle -> do
        hClose closeThisHandle
        action tmpPath (pipePath, pipeHandle)

commitOutput :: (FilePath, FilePath) -> FilePath -> IO ()
commitOutput (outfile, outpipe) outPath = do
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


dieLeft (Left err) = die err
dieLeft (Right val) = pure val

