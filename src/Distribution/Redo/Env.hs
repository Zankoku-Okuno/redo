module Distribution.Redo.Env
    ( Vars(..), varsFromEnv

    , TargetPath(..)
    , targetPath, parentPath
    , doesTargetExist
    , ProjDir(..), lookupProjDir
    , interpreterConfig
    , currentDepth

    , Scripts(..)
    , ScriptPath(..), TargetBasePath(..)
    , findScript

    , workerProcess
    ) where

import Distribution.Redo.Util

import System.IO
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit


data Vars = Vars {
      _target :: TargetPath
    , _parent :: Maybe TargetPath
    , _projDir :: ProjDir
    , _depth :: Int
    , _sh :: String
    , _shArgs :: [String]
    }

varsFromEnv :: FilePath -> IO Vars
varsFromEnv filepath = do
    _target <- targetPath filepath
    _parent <- parentPath
    _projDir <- lookupProjDir
                >>= maybe (die "no interpreter") pure
    _depth <- currentDepth
    (_sh, _shArgs) <- interpreterConfig _projDir >>=
                      maybe (die "no interpreter") pure
    -- package it up
    return Vars{..}


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
    return $ TargetPath <$> lookup _parentEnv env

doesTargetExist :: TargetPath -> IO Bool
doesTargetExist (TargetPath target) = doesFileExist target

newtype ProjDir = ProjDir FilePath

lookupProjDir :: IO (Maybe ProjDir)
lookupProjDir = do
    env <- getEnvironment
    (ProjDir <$>) <$> case lookup _projDirEnv env of
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


interpreterConfig :: ProjDir -> IO (Maybe (String, [String]))
interpreterConfig (ProjDir projDir) = do
    let shFile = projDir </> "interpreter.conf"
        argsFile = projDir </> "interpreter-args.conf"
    shFileExists <- doesFileExist shFile
    argsFileExists <- doesFileExist argsFile
    case (shFileExists, argsFileExists) of
        (True, True) -> do
            sh <- filter (/= '\n') <$> readFile shFile
            args <- lines <$> readFile argsFile
            return $ Just (sh, args)
        (True, False) -> do
            sh <- filter (/= '\n') <$> readFile shFile
            return $ Just (sh, [])
        (False, _) -> return Nothing


currentDepth :: IO Int
currentDepth = do
    env <- getEnvironment
    pure $ fromMaybe 0 $ read <$> lookup _depthEnv env


data Scripts = Scripts {
      _missing :: [ScriptPath]
    , _found :: Maybe ScriptPath
    , _targetBasePath :: Maybe TargetBasePath
    }
newtype ScriptPath = ScriptPath FilePath
newtype TargetBasePath = TargetBasePath FilePath

findScript :: TargetPath -> IO Scripts
findScript target = do
    let candidates = scriptCandidates target
    (map fst -> missing, firstFound -> script) <- breakM doesScriptExist candidates
    return $ uncurry (Scripts missing) script
    where
    doesScriptExist (ScriptPath sp, _) = doesFileExist sp
    firstFound (listToMaybe -> Nothing) = (Nothing, Nothing)
    firstFound (listToMaybe -> Just (sp, tbp)) = (Just sp, Just tbp)
    scriptCandidates :: TargetPath -> [(ScriptPath, TargetBasePath)]
    scriptCandidates (TargetPath target) = map wrap candidates
        where
        wrap (sp, tbp) = (ScriptPath sp, TargetBasePath tbp)
        candidates = apPair (dirpath </>) <$> (specificName : defaultNames)
        specificName = (filename <.> "do", takeBaseName filename)
        defaultNames = apFst mkDefault . swap <$> breakExts filename
        mkDefault ext = "default" <.> ext <.> "do"
        (dirpath, filename) = splitFileName target



workerProcess :: (MonadIO m) =>
    Vars
    -> Scripts
    -> (CreateProcess -> m ExitCode)
    -> m ExitCode
workerProcess Vars{..}
              Scripts{ _found = Just (ScriptPath script)
                     , _targetBasePath = Just (TargetBasePath targetBasePath)
                     }
              action = do
    let (TargetPath target) = _target
    env' <- liftIO $ do
        env0 <- getEnvironment
        return $ assocUpdate (_depthEnv, show $ _depth + 1)
               $ assocUpdate (_parentEnv, target)
                 env0
    withTmpFile _target $ \tmpfile tmpfp -> do
        --TODO add $1 argument
        let cmd = proc _sh $ _shArgs ++ [script, "", targetBasePath, tmpfile]
            process = cmd { std_out = UseHandle tmpfp
                          , env = Just env'
                          , cwd = Just $ takeDirectory target
                          }
        action process
    where
    withTmpFile :: (MonadIO m) => TargetPath -> (FilePath -> Handle -> m ExitCode) -> m ExitCode
    withTmpFile (TargetPath target) action = do
        (tmpfile, tmpfp) <- liftIO $ openTempFile "/tmp" "redo-.stdout"
        exitCode <- action tmpfile tmpfp
        liftIO $ do
            hClose tmpfp
            if exitCode == ExitSuccess
                then renameFile tmpfile target
                else removeFile tmpfile
        return exitCode


_depthEnv, _parentEnv, _projDirEnv :: String
_depthEnv = "REDODEPTH"
_parentEnv = "REDOPARENT"
_projDirEnv = "REDODIR"