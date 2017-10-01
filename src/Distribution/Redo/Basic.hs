module Distribution.Redo.Basic
    ( Project(..), openProject
    -- , Build(..), getBuild -- TODO it looks like I don't need a Build
    , Target(..), ScriptPath, getTarget
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import System.Environment
import System.FilePath
import System.Directory

import Data.ConfigFile

import Distribution.Redo.Error
import Distribution.Redo.State


-- | WARNING: best run with canonicalized paths
makeRelativeTo :: FilePath -> FilePath -> IO (Maybe FilePath)
makeRelativeTo containingDir filepath = do
    -- containingDir <- canonicalizePath containingDir
    -- filepath <- canonicalizePath filepath
    let relpath = containingDir `makeRelative` filepath
    pure $ if loop containingDir filepath
        then Just relpath
        else Nothing
    where
    loop dir test | dir == test = True
                  | otherwise = if next == test then False else loop dir next
                    where next = takeDirectory test


data Project = Project
    { state :: State

    , topDir :: FilePath
    , srcDir :: FilePath
    , buildDir :: FilePath
    , scriptDir :: FilePath

    , doScriptLang :: Maybe String
    , useNewArgs :: Bool
    -- TODO more configuration options
    -- where should we send logs?
    }
    deriving (Show)

openProject :: FilePath -> IO (Maybe Project)
openProject runningFrom = runMaybeT $ do
    topDir <- getProjectDirectory runningFrom
    liftIO $ canonicalizePath topDir >>= loadProject
    where
    getProjectDirectory :: FilePath -> MaybeT IO FilePath
    getProjectDirectory dir = do
        let nextDir = takeDirectory dir
        foundIt <- liftIO $ isStateAt dir
        if      foundIt        then pure dir
        else if nextDir == dir then mzero
        else                        getProjectDirectory nextDir
    loadProject :: FilePath -> IO Project
    loadProject topDir = do
        -- TODO is the database correctly formed?
        state <- loadState topDir
        
        let confPath = topDir </> ".redo" </> "redo.conf"
        confExists <- doesFileExist confPath
        unless confExists $ do
            writeFile confPath ""
        mkProject <- runExceptT $ do
            let getDef :: (MonadError CPError m, Get_C a) => ConfigParser -> SectionSpec -> OptionSpec -> a -> m a
                getDef cp s o d = if has_option cp s o then get cp s o else return d -- TODO check when this makes it into the ConfigFile source
                emptyToNothing "" = Nothing
                emptyToNothing x = Just x
            config <- join $ liftIO $ readfile emptyCP confPath
            
            srcDir    <- (topDir </>) <$> getDef config "directories" "src"    ""
            buildDir  <- (topDir </>) <$> getDef config "directories" "out"    ""
            scriptDir <- (topDir </>) <$> getDef config "directories" "script" ""

            doScriptLang <- emptyToNothing <$> getDef config "script" "language" ""
            useNewArgs <- getDef config "script" "newArgs" False

            return $ \state -> Project {..}
        pure $ case mkProject of
            Right mkProject -> mkProject state
            Left err -> error "INTERNAL ERROR (please report): could not open or create config file"

data Build = Build {
      buildId :: BuildId
    -- TODO build options &c
    }
    deriving (Show)

getBuild :: Project -> IO Build
getBuild Project{..} = do
    buildId <- loadBuild state
    pure Build{..}

data Target = Target
    { canonPath :: FilePath
    , child :: Maybe FilePath -- NOT called child b/c this target is reponsbile for creating stuff for the child
    , srcPath :: FilePath
    , outPath :: FilePath
    , scriptPath :: Maybe ScriptPath
    , counterfactualScripts :: [FilePath]
    }
    deriving (Show)
type ScriptPath = (FilePath, (BaseName, Extension))

type BaseName = String
type Extension = String
getTarget :: Project -> FilePath -> IO Target
getTarget Project{..} request = do
    inSrcDir <- makeRelativeTo srcDir request
    child <- lookupEnv "REDO__TARGET"
    let canonPath = case child of
            (Just (takeDirectory' -> childDir)) -> childDir </> request
            Nothing -> case inSrcDir of
                Just filepath -> filepath
                Nothing -> request

    -- NOTE this ensures that the path is within the realm of validity for canonPaths
    filepath <- canonicalizePath (srcDir </> canonPath)
    makeRelativeTo srcDir filepath >>= \case
        Nothing -> throw $ TargetNotFound canonPath
        Just canonPath -> do
            let srcPath = srcDir </> canonPath
                outPath = buildDir </> canonPath
                scriptPaths = candidateScripts scriptDir canonPath
            (scriptPath, counterfactualScripts) <- loadScriptPath (srcPath, scriptPaths)
            pure Target{..}
    where
    candidateScripts :: FilePath -> FilePath -> [(FilePath, (BaseName, Extension))]
    candidateScripts scriptDir canonPath = directScript : defaultScripts
        where
        directScript = (scriptDir </> canonPath <.> "do", head candidateExtensions)
        defaultScripts = [contextualize dir script | dir <- defaultDirs, script <- defaultBaseNames]
            where
            contextualize dir (base, extInfo) = (scriptDir </> dir </> base, extInfo)
            defaultBaseNames = [ ("default" <.> ext <.> "do", extInfo) |
                      extInfo@(_, ext) <- candidateExtensions
                    , not (null ext)
                    ]
            defaultDirs = loop (takeDirectory' canonPath)
                where
                loop "" = [""]
                loop dir = dir : loop (takeDirectory' dir)
        candidateExtensions :: [(BaseName, Extension)]
        candidateExtensions = reverse $ ((filename, ""):) $ loop "" filename
            where
            filename = takeFileName canonPath
            loop exts filename = case splitExtension filename of
                ("", _) -> []
                (baseName, "") -> []
                (baseName, ext) -> (baseName, ext <.> exts) : loop (ext <.> exts) baseName
    loadScriptPath :: (FilePath, [(FilePath, (BaseName, Extension))]) -> IO (Maybe ScriptPath, [FilePath])
    loadScriptPath (srcPath, candidatePaths) = do
        srcExists <- doesFileExist srcPath
        (theScript, counterfactualScripts) <- whichFilesExist candidatePaths
        let scriptPath = case (srcExists, theScript) of
                            (_,     Just (theScript, extInfo)) -> Just (theScript, extInfo)
                            (True,  Nothing)                   -> Nothing
                            (False, Nothing)                   -> throw $ ScriptNotFound srcPath (fst <$> candidatePaths)
        pure (scriptPath, counterfactualScripts)
        where
        whichFilesExist :: [(FilePath, (BaseName, Extension))] -> IO (Maybe (FilePath, (BaseName, Extension)), [FilePath])
        whichFilesExist [] = pure (Nothing, [])
        whichFilesExist ((filepath, extInfo) : rest) = do
            exists <- doesFileExist filepath
            if exists
            then pure (Just (filepath, extInfo), [])
            else whichFilesExist rest >>= \(answer, rest) -> pure (answer, filepath : rest)

takeDirectory' :: FilePath -> FilePath
takeDirectory' filepath = let it = takeDirectory filepath in if it == "." then "" else it