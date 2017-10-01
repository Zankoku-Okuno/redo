module Distribution.Redo.State
    ( State, BuildId
    , isStateAt
    , loadState, initState
    , loadBuild, finishBuild
    , registerSource, registerTarget, registerScript
    , markBuilding, markUpToDate, markBuildFailed
    , allDependencies, checkChangedByTime, checkChangedByHash
    , addIfCreateDependency, addIfChangeDependency, clearDependencies
    ) where

import Prelude hiding (init)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.Time.Clock
import System.FilePath
import System.Directory
import Database.SQLite.Simple as Sql


newtype State = State { conn :: Sql.Connection }
instance Show State where
    show _ = "<State>"

isStateAt :: FilePath -> IO Bool
isStateAt testDir =
    doesDirectoryExist (testDir </> ".redo")


loadState :: FilePath -> IO State
loadState topDir = do
    let dbfile = topDir </> ".redo" </> "redo.sqlite"
    conn <- Sql.open dbfile
    pure $ State conn

initState :: FilePath -> IO State
initState topDir = do
    let redoDir = topDir </> ".redo"
    createDirectoryIfMissing False redoDir
    conn <- Sql.open $ redoDir </> "redo.sqlite"
    Sql.withTransaction conn $ do
        Sql.execute_ conn "CREATE TABLE version (number STRING NOT NULL);"
        Sql.execute_ conn "INSERT INTO version (number) VALUES ('0');"
        Sql.execute_ conn
            "CREATE TABLE build (\n\
            \    id        INTEGER PRIMARY KEY\n\
            \  , lifecycle TEXT\n\
            \                CHECK (lifecycle IN ('ACTIVE', 'DONE'))\n\
            \ -- TODO columns to cache the config for a build\n\
            \);"
        -- FIXME allow for external files. e.g. if you depend on a system library (obviously not source) then you should rebuild when that lib changes
        Sql.execute_ conn
            "CREATE TABLE file (\n\
            \    id         INTEGER PRIMARY KEY\n\
            \  , filename   TEXT NOT NULL UNIQUE\n\
            \  , filetype   TEXT NOT NULL\n\
            \                 CHECK (filetype IN ('SOURCE', 'TARGET', 'SCRIPT'))\n\
            \  , lifecycle  TEXT\n\
            \                 CHECK (lifecycle IN ('BUILDING', 'UPTODATE', 'BUILDFAILED'))\n\
            \  , last_scan  INTEGER REFERENCES build(id)\n\
            \  , last_built TEXT\n\
            \  , last_hash  TEXT\n\
            \);"
        Sql.execute_ conn "CREATE INDEX file_filename_idx ON file (filename);"
        Sql.execute_ conn
            "CREATE TABLE dependency (\n\
            \    child      INTEGER NOT NULL REFERENCES file(id)\n\
            \  , depends_on INTEGER NOT NULL REFERENCES file(id)\n\
            \  , dep_type   TEXT NOT NULL CHECK (dep_type IN ('IFCREATE', 'IFCHANGE'))\n\
            \);"
    loadState topDir


type BuildId = Int

-- WARNING must be run inside transaction
getBuildId_ :: Sql.Connection -> IO Int
getBuildId_ conn = do
    Sql.query_ conn "SELECT id FROM build WHERE lifecycle = 'ACTIVE';" >>= \case
        [Only buildId] -> pure buildId
        [] -> do
            Sql.execute_ conn "INSERT INTO build (lifecycle) VALUES ('ACTIVE');"
            Sql.query_ conn "SELECT last_insert_rowid();" >>= \case
                [Only buildId] -> pure buildId
                _ -> error "INTERNAL ERROR (please report): could not create new build"
        (_ :: [Only Int]) -> error "INTERNAL ERROR (please report): starting build when one is already active"

loadBuild :: State -> IO BuildId
loadBuild State{..} = Sql.withTransaction conn $ getBuildId_ conn

finishBuild :: State -> IO ()
finishBuild State{..} = Sql.withTransaction conn $ do
    Sql.execute_ conn "UPDATE build SET lifecycle = 'DONE' WHERE lifecycle = 'ACTIVE';"


registerSource :: State -> FilePath -> IO ()
registerSource = register_ "SOURCE"

registerTarget :: State -> FilePath -> IO ()
registerTarget = register_ "TARGET"

registerScript :: State -> FilePath -> IO ()
registerScript = register_ "SCRIPT"

register_ :: String -> State -> FilePath -> IO ()
register_ filetype State{..} filename = Sql.withTransaction conn $ do
    Sql.query conn "SELECT count(*) FROM file WHERE filename = ?;" (Only filename) >>= \case
        -- FIXME what if a file is registered as two different types?
            -- when the last scan isn't this build, overwrite
            -- when state is null, overwrite
            -- when state is running, error
            -- TODO what about other states?
        [Only (0 :: Int)] -> Sql.execute conn "INSERT INTO file (filename, filetype) VALUES (?, ?);" (filename, filetype)
        [Only _] -> pure ()
        _ -> error "INTERNAL ERROR (please report): could not resgister file"

markBuilding :: State -> FilePath -> IO ()
markBuilding State{..} filename = Sql.withTransaction conn $ do
    buildId <- getBuildId_ conn
    let q = "UPDATE file SET\n\
            \    lifecycle = 'BUILDING'\n\
            \  , last_scan = ?\n\
            \WHERE filename = ?;"
    Sql.execute conn q (buildId, filename)

markUpToDate :: State -> (FilePath, Maybe UTCTime, Maybe BS.ByteString)-> IO ()
markUpToDate State{..} (filename, time, hash) = Sql.withTransaction conn $ do
    let q = "UPDATE file SET\n\
            \    lifecycle = 'UPTODATE'\n\
            \  , last_built = ?\n\
            \  , last_hash = ?\n\
            \WHERE filename = ?;"
    Sql.execute conn q (time, Hex.encode <$> hash, filename)

markBuildFailed :: State -> FilePath -> IO ()
markBuildFailed State{..} filename = Sql.withTransaction conn $ do
    let q = "UPDATE file SET\n\
            \    lifecycle = 'BUILDFAILED'\n\
            \WHERE filename = ?;"
    Sql.execute conn q (Only filename)


allDependencies :: State -> FilePath -> IO [(String, FilePath)] -- FIXME this string should be an enum
allDependencies State{..} canonPath = do
    let q = "WITH RECURSIVE deps(id) AS\n\
            \    (   SELECT id FROM file WHERE filename = ?\n\
            \    UNION\n\
            \        SELECT depends_on\n\
            \        FROM dependency, deps\n\
            \        WHERE child = deps.id\n\
            \    )\n\
            \SELECT filetype, filename FROM file WHERE id IN deps;"
    Sql.query conn q (Only canonPath)


checkChangedByTime :: State -> FilePath -> Maybe UTCTime -> IO Bool
checkChangedByTime State{..} filepath checkTime = do
    let q = "SELECT last_built FROM file WHERE filename = ?;"
    storedTime <- Sql.query conn q (Only filepath) >>= pure . \case
        [] -> Nothing
        [Only Nothing] -> Nothing
        [Only last_built] -> last_built
        _ -> error "INTERNAL ERROR (please report): corrupted file change log"
    pure $ case (checkTime, storedTime) of
        (Just checkTime, Just storedTime) -> storedTime < checkTime
        (Nothing, Nothing) -> False
        _ -> True


checkChangedByHash :: State -> FilePath -> Maybe BS.ByteString -> IO Bool
checkChangedByHash State{..} filepath checkHash = do
    let q = "SELECT last_hash FROM file WHERE filename = ?;"
    storedHash <- Sql.query conn q (Only filepath) >>= pure . \case
        [] -> Nothing
        [Only Nothing] -> Nothing
        [Only (Just (Hex.decode -> (last_hash, undecoded)))]
            | undecoded == "" -> Just last_hash
            | otherwise -> error "INTERNAL ERROR (please report): corrupted hash"
        _ -> error "INTERNAL ERROR (please report): corrupted file change log"
    pure $ case (checkHash, storedHash) of
        (Just checkHash, Just storedHash) -> storedHash /= checkHash
        (Nothing, Nothing) -> False
        _ -> True


clearDependencies :: State -> FilePath -> IO ()
clearDependencies state@State{..} filename = Sql.withTransaction conn $ do
    let q = "DELETE FROM dependency\n\
            \WHERE child IN (\n\
            \   SELECT id FROM file\n\
            \   WHERE file.filename = ?\n\
            \);"
    Sql.execute conn q (Only filename)

addIfCreateDependency :: State -> (FilePath, FilePath) -> IO ()
addIfCreateDependency = addDependency_ "IFCREATE"

addIfChangeDependency :: State -> (FilePath, FilePath) -> IO ()
addIfChangeDependency = addDependency_ "IFCHANGE"



addDependency_ :: String -> State -> (FilePath, FilePath) -> IO ()
addDependency_ dep_type state@State{..} (child, depends_on) = Sql.withTransaction conn $ do
    child <- Sql.query conn "SELECT id FROM file WHERE filename = ?" (Only child)
    depends_on <- Sql.query conn "SELECT id FROM file WHERE filename = ?" (Only depends_on)
    case (child, depends_on) of
        ([Only (child :: Int)], [Only (depends_on :: Int)]) -> do
            let q = "INSERT INTO dependency (child, depends_on, dep_type)\n\
                    \VALUES (?, ?, ?);"
            Sql.execute conn q (child, depends_on, dep_type)
        _ -> error "INTERNAL ERROR (please report): unable to log dependency information"
