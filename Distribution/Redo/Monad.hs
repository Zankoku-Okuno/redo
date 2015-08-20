{-# LANGUAGE LambdaCase, ViewPatterns, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Distribution.Redo.Monad (
      Redo, runRedo
    , Vars(..), mkVars
    , Scripts(..), findScript, ScriptPath, TargetBasePath
    , Result(..), mkProcess

    , ProjDir, lookupProjDir, mkSkeleton, cleanSkeleton
    , TargetPath, targetPath, parentPath, doesTargetExist

    , Status(..), status
    , DepPath(..)
    , getDeps, addDep, clearDeps
    , checkForChanges
    , recordChange, recordUpToDate, recordBuildFailure

    , debug
    ) where

import Distribution.Redo.Util
import Distribution.Redo.Hash

import Control.Monad.Trans
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import System.IO
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit as X



newtype Redo a = Redo { unRedo :: ReaderT Vars IO a }
    deriving(Functor, Applicative, Monad, MonadIO)

instance MonadReader Vars Redo where
    ask = Redo ask
    local f action = Redo $ local f (unRedo action)

runRedo :: Redo a -> Vars -> IO a
runRedo action vars = flip runReaderT vars $ unRedo action

data Vars = Vars {
      _target :: TargetPath
    , _parent :: Maybe TargetPath
    , _projDir :: ProjDir
    , _depth :: Int
    , _sh :: String
    , _shArgs :: [String]
    }

mkVars :: FilePath -> IO Vars
mkVars filepath = do
    target <- targetPath filepath
    parent <- parentPath
    projDir <- lookupProjDir >>= \case
        Nothing -> die "cannot find redo project"
        Just it -> return it
    env <- getEnvironment
    let depth = fromMaybe 0 $ read <$> lookup "REDODEPTH" env
    (sh, shArgs) <- interpreterConfig projDir
    -- package it up
    return Vars {
          _target = target
        , _projDir = projDir
        , _sh = sh
        , _shArgs = shArgs
        , _depth = depth
        , _parent = parent
    }

data Scripts = Scripts {
      _missing :: [ScriptPath]
    , _found :: Maybe ScriptPath
    , _targetBasePath :: Maybe TargetBasePath
    }
newtype ScriptPath = ScriptPath FilePath
newtype TargetBasePath = TargetBasePath FilePath

findScript :: TargetPath -> Redo Scripts
findScript (TargetPath target) = liftIO $ do
    (map fst -> missing, other) <- breakM (\(ScriptPath sp, _) -> doesFileExist sp) scriptCandidates
    return $ uncurry (Scripts missing) $ justPair (listToMaybe other)
    where
    justPair (Just (a, b)) = (Just a, Just b)
    justPair Nothing = (Nothing, Nothing)
    scriptCandidates :: [(ScriptPath, TargetBasePath)]
    scriptCandidates = map wrap candidates
        where
        wrap (tbp, sp) = (ScriptPath sp,TargetBasePath tbp)
        candidates = map (apPair (dirpath </>)) $ specificName : defaultNames
        specificName = (takeBaseName filename, filename ++ ".do")
        defaultNames = map (apSnd mkDefault) $ breakExts filename
        mkDefault ext = "default" ++ ext ++ ".do"
        (dirpath, filename) = splitFileName target


data Result = Skip | Run ExitCode | Bad String

mkProcess :: ScriptPath -> Maybe TargetBasePath
          -> (CreateProcess -> Redo ExitCode)
          -> Redo ExitCode
mkProcess (ScriptPath script) targetBasePath_m action =
    withTmpFile $ \tmpfile tmpfp -> do
        TargetPath target <- asks _target
        let targetBasePath =
                case targetBasePath_m of
                    Just (TargetBasePath it) -> it
                    Nothing -> ""
        env' <- do
            depth <- asks _depth 
            env0 <- liftIO getEnvironment
            return $ assocUpdate ("REDODEPTH", show $ depth + 1)
                   $ assocUpdate ("REDOPARENT", target)
                     env0
        interpreter <- asks _sh
        userargs <- asks _shArgs
        --TODO add $1 argument
        let cmd = proc interpreter $ userargs ++ [script, "", targetBasePath, tmpfile]
            process = cmd { std_out = UseHandle tmpfp
                          , env = Just env'
                          , cwd = Just $ takeDirectory target
                          }
        action process
    where
    withTmpFile :: (FilePath -> Handle -> Redo ExitCode) -> Redo ExitCode
    withTmpFile action = do
        (tmpfile, tmpfp) <- liftIO $ openTempFile "/tmp" "redo-.stdout"
        exitCode <- action tmpfile tmpfp
        (TargetPath target) <- asks _target
        liftIO $ do
            hClose tmpfp
            if exitCode == ExitSuccess
                then renameFile tmpfile target
                else removeFile tmpfile
        return exitCode


newtype ProjDir = ProjDir FilePath

lookupProjDir :: IO (Maybe ProjDir)
lookupProjDir = do
    env <- getEnvironment
    (ProjDir <$>) <$> case lookup "REDODIR" env of
        Nothing -> candidates
        Just dir -> return (Just dir)
    where
    candidates :: IO (Maybe FilePath)
    candidates = do
        cwd <- getCurrentDirectory
        let parents = onPathSegments (takeWhile (not . null) . iterate init) cwd
            candidates = map (</> ".redo") parents
        found_m <- filterM doesDirectoryExist candidates
        return $ listToMaybe found_m
        where
        onPathSegments f = map joinPath . f . splitPath

mkSkeleton :: Redo ()
mkSkeleton = do
    (ProjDir projDir) <- asks _projDir
    liftIO $ do
        createDirectoryIfMissing False $ projDir </> "deps"
        createDirectoryIfMissing False $ projDir </> "hashes"
        do
            exists <- doesDirectoryExist $ projDir </> "utd"
            when exists $ removeDirectoryRecursive $ projDir </> "utd"
            createDirectoryIfMissing False $ projDir </> "utd"
        createDirectoryIfMissing False $ projDir </> "fails"

cleanSkeleton :: Redo ()
cleanSkeleton = do
    (ProjDir projDir) <- asks _projDir
    liftIO $ do
        depsExists <- doesDirectoryExist $ projDir </> "deps"
        when depsExists $ removeDirectoryRecursive $ projDir </> "deps"
        hashesExists <- doesDirectoryExist $ projDir </> "hashes"
        when hashesExists $ removeDirectoryRecursive $ projDir </> "hashes"
        utdExists <- doesDirectoryExist $ projDir </> "utd"
        when utdExists $ removeDirectoryRecursive $ projDir </> "utd"
        failsExists <- doesDirectoryExist $ projDir </> "fails"
        when failsExists $ removeDirectoryRecursive $ projDir </> "fails"


newtype TargetPath = TargetPath FilePath

instance Show TargetPath where
    show (TargetPath target) = show target

targetPath :: FilePath -> IO TargetPath
targetPath target = do
    abspath <- makeAbsolute target
    let (absdir, absname) = splitFileName abspath
    cannonPath <- canonicalizePath absdir --FIXME this will fail if the directry doesn't exist, which is probably good, but I need a better diagnostic
    return $ TargetPath $ cannonPath </> absname

parentPath :: IO (Maybe TargetPath)
parentPath = do
    env <- getEnvironment
    return $ TargetPath <$> lookup "REDOPARENT" env

doesTargetExist :: TargetPath -> Redo Bool
doesTargetExist (TargetPath target) = liftIO $ doesFileExist target




data Status = UpToDate | Failed | Uncomputed

status :: TargetPath -> Redo Status
status (TargetPath targetPath) = do
    utd <- liftIO . doesFileExist =<< mkUTDFile targetPath
    failed <- liftIO . doesFileExist =<< mkFailFile targetPath
    return $ case (utd, failed) of
        (_, True) -> Failed
        (True, _) -> UpToDate
        _ -> Uncomputed


data DepPath = TargetDep TargetPath
             | ScriptDep ScriptPath

getDeps :: TargetPath -> Redo [TargetPath]
getDeps (TargetPath target) = do
    depsFile <- mkDepsFile target
    exists <- liftIO $ doesFileExist depsFile
    liftIO $ if exists then map TargetPath . lines <$> readFile depsFile else return []

addDep :: TargetPath -> DepPath -> Redo ()
addDep (TargetPath parent) (ScriptDep (ScriptPath child)) = do
    depsFile <- mkDepsFile parent
    liftIO $ withFile depsFile AppendMode $ flip hPutStrLn child
    hashFile <- mkHashFile child
    scriptHash <- liftIO $ hashContents child
    liftIO $ writeHashFile hashFile scriptHash
addDep (TargetPath parent) (TargetDep (TargetPath child)) = do
    depsFile <- mkDepsFile parent
    liftIO $ withFile depsFile AppendMode $ flip hPutStrLn child

clearDeps :: TargetPath -> Redo ()
clearDeps (TargetPath target) = do
    depsFile <- mkDepsFile target
    liftIO $ removeFileIfExists depsFile

checkForChanges :: TargetPath -> Redo Bool
checkForChanges (TargetPath target) = do
    hashFile <- mkHashFile target
    hashExists <- liftIO $ doesFileExist hashFile
    if not hashExists then return True else do
        lastHash <- liftIO $ readHashFile hashFile
        currentHash <- liftIO $ hashContents target
        --debug $ "last hash: " ++ show lastHash
        --debug $ "curr hash: " ++ show currentHash
        return $ currentHash /= lastHash

recordChange :: TargetPath -> Redo ()
recordChange (TargetPath target) = do
    hashFile <- mkHashFile target
    liftIO $ writeHashFile hashFile =<< hashContents target

recordUpToDate :: TargetPath -> Redo ()
recordUpToDate (TargetPath target) = do
    liftIO . removeFileIfExists =<< mkFailFile target
    exists <- liftIO $ doesFileExist target
    when exists $ do
        liftIO . flip writeFile "" =<< mkUTDFile target

recordBuildFailure :: TargetPath -> Redo ()
recordBuildFailure (TargetPath target) = do
    liftIO . removeFileIfExists =<< mkUTDFile target
    liftIO . flip writeFile "" =<< mkFailFile target


--FIXME use proper logging libraries
debug :: String -> Redo ()
debug msg = do
    depth <- asks _depth
    let indentedMsg = concat (replicate depth "    ") ++ msg
    liftIO $ putErrLn indentedMsg





interpreterConfig :: ProjDir -> IO (String, [String])
interpreterConfig (ProjDir projDir) = do
    let shFile = projDir </> "interpreter.conf"
        argsFile = projDir </> "interpreter-args.conf"
    shFileExists <- doesFileExist shFile
    argsFileExists <- doesFileExist argsFile
    case (shFileExists, argsFileExists) of
        (True, True) -> do
            sh <- filter (/= '\n') <$> readFile shFile
            args <- lines <$> readFile argsFile
            return (sh, args)
        (True, False) -> do
            sh <- filter (/= '\n') <$> readFile shFile
            return (sh, [])
        (False, _) -> die "no interpreter"


mkHashFile :: FilePath -> Redo FilePath
mkHashFile = mkFooFile "hashes"
mkDepsFile :: FilePath -> Redo FilePath
mkDepsFile = mkFooFile "deps"
mkUTDFile :: FilePath -> Redo FilePath
mkUTDFile = mkFooFile "utd"
mkFailFile :: FilePath -> Redo FilePath
mkFailFile = mkFooFile "fails"

mkFooFile :: String -> FilePath -> Redo FilePath
mkFooFile foo path = do
    let hash = hash16utf8 path
    (ProjDir projDir) <- asks _projDir
    return $ projDir </> foo </> show hash


