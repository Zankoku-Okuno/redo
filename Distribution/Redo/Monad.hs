{-# LANGUAGE LambdaCase, ViewPatterns, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Distribution.Redo.Monad (
      module X

    , Result(..)
    , Redo, runRedo
    , Vars(..)
    , Scripts(..)

    --FIXME do not export constructors
    , ProjDir, TargetPath, ScriptPath, TargetBasePath, TmpPath
    , DepPath(..)

    , targetPath, parentPath, lookupProjDir, redoDepth, interpreter
    
    , mkSkeleton, cleanSkeleton

    , getDeps, addDep, clearDeps
    , recordChange, recordUpToDate, recordBuildFailure
    , checkForChanges

    , findScript

    , withTmpFile
    , mkScriptArgs, mkScriptEnv, mkScriptCwd

    , doesTargetExist
    
    , mkHashFile, mkDepsFile, mkUTDFile, mkFailFile
    ) where

import Distribution.Redo.Util
import Distribution.Redo.Hash

import Control.Monad.Trans
import Control.Monad.Reader as X

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import System.IO
import System.Environment
import System.Directory
import System.FilePath
import System.Exit as X


data Result = Skip | Run ExitCode | Bad String

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

data Scripts = Scripts {
      _missing :: [ScriptPath]
    , _found :: Maybe ScriptPath
    , _targetBasePath :: Maybe TargetBasePath
    }


newtype TargetPath = TargetPath FilePath
newtype ProjDir = ProjDir FilePath
newtype ScriptPath = ScriptPath FilePath
newtype TargetBasePath = TargetBasePath FilePath
newtype TmpPath = TmpPath FilePath
data DepPath = TargetDep TargetPath
             | ScriptDep ScriptPath

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

redoDepth :: IO Int
redoDepth = do
    env <- getEnvironment
    return $ fromMaybe 0 $ read <$> lookup "REDODEPTH" env

interpreter :: ProjDir -> IO (String, [String])
interpreter (ProjDir projDir) = do
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


recordChange :: TargetPath -> Redo ()
recordChange (TargetPath target) = do
    hashFile <- mkHashFile (TargetPath target)
    liftIO $ writeHashFile hashFile =<< hashContents target

recordUpToDate :: TargetPath -> Redo ()
recordUpToDate target = do
    liftIO . removeFileIfExists =<< mkFailFile target
    exists <- liftIO $ doesTargetExist target
    when exists $ do
        liftIO . flip writeFile "" =<< mkUTDFile target

recordBuildFailure :: TargetPath -> Redo ()
recordBuildFailure (TargetPath target) = do
    liftIO . removeFileIfExists =<< mkUTDFile (TargetPath target)
    liftIO . flip writeFile "" =<< mkFailFile (TargetPath target)









doesTargetExist :: TargetPath -> IO Bool
doesTargetExist (TargetPath target) = doesFileExist target

findScript :: TargetPath -> IO Scripts
findScript (TargetPath target) = do
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

mkScriptArgs :: ScriptPath -> Maybe TargetBasePath -> TmpPath -> [String]
mkScriptArgs (ScriptPath script) targetBasePath_m (TmpPath tmpfile) = 
    let (TargetBasePath targetBasePath) = fromMaybe (TargetBasePath "") targetBasePath_m
    in  [script, "", targetBasePath, tmpfile]
        --TODO add $1 argument

mkScriptEnv :: Redo [(String, String)]
mkScriptEnv = do
    (TargetPath target) <- asks _target
    depth <- asks _depth 
    env0 <- liftIO getEnvironment
    let env' = assocUpdate ("REDODEPTH", show $ depth + 1)
             $ assocUpdate ("REDOPARENT", target)
               env0
    return env'

mkScriptCwd :: Redo FilePath
mkScriptCwd = do
    TargetPath target <- asks _target
    return $ takeDirectory target

withTmpFile :: (TmpPath -> Handle -> Redo ExitCode) -> Redo ExitCode
withTmpFile action = do
    (pretmpfile, tmpfp) <- liftIO $ openTempFile "/tmp" "redo-.stdout"
    let tmpfile = TmpPath pretmpfile
    exitCode <- action tmpfile tmpfp
    (TargetPath target) <- asks _target
    liftIO $ do
        hClose tmpfp
        if exitCode == ExitSuccess
            then renameFile pretmpfile target
            else removeFile pretmpfile
    return exitCode



getDeps :: TargetPath -> Redo [TargetPath]
getDeps target = do
    depsFile <- mkDepsFile target
    exists <- liftIO $ doesFileExist depsFile
    liftIO $ if exists then map TargetPath . lines <$> readFile depsFile else return []

addDep :: TargetPath -> DepPath -> Redo ()
addDep parent (ScriptDep (ScriptPath child)) = do
    depsFile <- mkDepsFile parent
    liftIO $ withFile depsFile AppendMode $ flip hPutStrLn child
    hashFile <- mkHashFile (TargetPath child)
    scriptHash <- liftIO $ hashContents child
    liftIO $ writeHashFile hashFile scriptHash
addDep parent (TargetDep (TargetPath child)) = do
    depsFile <- mkDepsFile parent
    liftIO $ withFile depsFile AppendMode $ flip hPutStrLn child

clearDeps :: TargetPath -> Redo ()
clearDeps (TargetPath target) = do
    depsFile <- mkDepsFile (TargetPath target)
    liftIO $ removeFileIfExists depsFile


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



mkHashFile :: TargetPath -> Redo FilePath
mkHashFile = mkFooFile "hashes"
mkDepsFile :: TargetPath -> Redo FilePath
mkDepsFile = mkFooFile "deps"
mkUTDFile :: TargetPath -> Redo FilePath
mkUTDFile = mkFooFile "utd"
mkFailFile :: TargetPath -> Redo FilePath
mkFailFile = mkFooFile "fails"

mkFooFile :: String -> TargetPath -> Redo FilePath
mkFooFile foo (TargetPath path) = do
    let hash = hash16utf8 path
    (ProjDir projDir) <- asks _projDir
    return $ projDir </> foo </> show hash

checkForChanges :: TargetPath -> Redo Bool
checkForChanges (TargetPath target) = do
    hashFile <- mkHashFile (TargetPath target)
    hashExists <- liftIO $ doesFileExist hashFile
    if not hashExists then return True else do
        lastHash <- liftIO $ readHashFile hashFile
        currentHash <- liftIO $ hashContents target
        --debug $ "last hash: " ++ show lastHash
        --debug $ "curr hash: " ++ show currentHash
        return $ currentHash /= lastHash