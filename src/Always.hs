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
    build@Build{..} <- getBuild project
    target@Target{..} <- project `getTarget` request
    
    hPutStrLn stderr $ "INFO: requested `" ++ show request ++ "`"
    hPutStrLn stderr $ "INFO: project is " ++ show project
    hPutStrLn stderr $ "INFO: build is " ++ show build
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
    in do
        setup
        body `onException` cleanAfterError

runDoScript :: Project -> Target -> IO ()
runDoScript Project{..} Target{..} = case scriptPath of
    Just (scriptPath, (baseName, _)) -> do
        ensureDestination
        withSystemTempFile (takeFileName outPath)  $ \tmpPath delme -> do
            hClose delme
            -- FIXME if script not executable, then choose the default scripting language
            let newEnv = [("REDO__TARGET", canonPath)] -- TODO do I need any more env vars?
            env <- (newEnv ++) <$> getEnvironment
            (tmpRead, tmpWrite) <- createPipe
            let args = [outPath, baseName, tmpPath] -- FIXME implement the newArgs option
                cmd = case doScriptLang of
                    Nothing -> proc scriptPath args
                    Just sh -> shell $ sh ++ (' ':scriptPath) ++ concatMap (' ':) args
                process = cmd {
                      cwd = Just outDir
                    , env = Just env
                    , std_in = Inherit
                    -- FIXME use a separate handle for stdout, then later check that only one of the handles has been written to
                    , std_out = UseHandle tmpWrite
                    , std_err = Inherit
                }
            withCreateProcess process $ \procIn procOut procErr procHandle -> do
                exitCode <- waitForProcess procHandle
                case exitCode of
                    ExitSuccess -> do
                        -- FIXME is reading the entire thing really necessary?
                        fromFile <- readFile tmpPath
                        fromStdout <- hGetContents tmpRead
                        case (null fromFile, null fromStdout) of
                            (False, False) -> throw DoubleOutput
                            (False, True) -> writeFile outPath fromStdout
                            (True, _) -> copyFile tmpPath outPath -- FIXME this should be moveFile, if that won't mess `withSystemTempFile`
                        
                    ExitFailure code -> throw $ DoScriptFailure code
    Nothing -> do
        when (outPath /= srcPath) $ do
            ensureDestination
            copyFile srcPath outPath
    where
    ensureDestination = createDirectoryIfMissing True outDir
    outDir = takeDirectory outPath


dieLeft (Left err) = die err
dieLeft (Right val) = pure val

