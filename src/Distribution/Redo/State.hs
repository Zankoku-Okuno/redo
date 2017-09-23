module Distribution.Redo.State
    ( State, BuildId
    , isStateAt
    , loadState, initState
    , loadBuild, newBuild
    , registerSource, registerTarget, registerScript
    , markBuilding, markUpToDate, markBuildFailed
    , addIfCreateDependency, addIfChangeDependency, clearDependencies
    ) where

import Prelude hiding (init)

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
        Sql.execute_ conn
            "CREATE TABLE file (\n\
            \    id        INTEGER PRIMARY KEY\n\
            \  , filename  TEXT NOT NULL UNIQUE\n\
            \  , filetype  TEXT NOT NULL\n\
            \                CHECK (filetype IN ('SOURCE', 'TARGET', 'SCRIPT'))\n\
            \  , lifecycle TEXT NOT NULL DEFAULT 'UNSCANNED'\n\
            \                CHECK (lifecycle IN ('UNSCANNED', 'BUILDING', 'UPTODATE', 'BUILDFAILED'))\n\
            \ -- TODO timestamp, hash of contents\n\
            \);"
        Sql.execute_ conn "CREATE INDEX file_filename_idx ON file (filename);"
        Sql.execute_ conn
            "CREATE TABLE dependency (\n\
            \    child      INTEGER NOT NULL REFERENCES file(id)\n\
            \  , depends_on INTEGER NOT NULL REFERENCES file(id)\n\
            \  , dep_type   TEXT NOT NULL CHECK (dep_type IN ('IFCREATE', 'IFCHANGE'))\n\
            \);"
    -- TODO plop down an empty config file
    loadState topDir


type BuildId = Int

loadBuild :: State -> IO (Maybe BuildId)
loadBuild State{..} = Sql.withTransaction conn $ do
    Sql.query_ conn "SELECT id FROM build WHERE lifecycle = 'ACTIVE';" >>= \case
        [] -> pure Nothing
        [Only buildId] -> pure $ Just buildId
        _ -> error "INTERNAL ERROR (please report): multiple builds active simultaneously"

newBuild :: State -> IO BuildId
newBuild State{..} = Sql.withTransaction conn $ do
    Sql.query_ conn "SELECT id FROM build WHERE lifecycle = 'ACTIVE';" >>= \case
        [] -> do
            Sql.execute_ conn "INSERT INTO build (lifecycle) VALUES ('ACTIVE');"
            Sql.query_ conn "SELECT last_insert_rowid();" >>= \case
                [Only buildId] -> do
                    Sql.execute_ conn "UPDATE file SET lifecycle = 'UNSCANNED';"
                    pure buildId
                _ -> error "INTERNAL ERROR (please report): could not create new build"
        (_ :: [Only Int]) -> error "INTERNAL ERROR (please report): starting build when one is already active"


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
            -- when state is unscanned, overwrite
            -- when state is running, error
            -- TODO what about other states?
        [Only (0 :: Int)] -> Sql.execute conn "INSERT INTO file (filename, filetype) VALUES (?, ?);" (filename, filetype)
        [Only _] -> pure ()
        _ -> error "INTERNAL ERROR (please report): could not resgister file"

markBuilding :: State -> FilePath -> IO ()
markBuilding state@State{..} filename = Sql.withTransaction conn $ do
    Sql.execute conn "UPDATE file SET lifecycle = 'BUILDING' WHERE filename = ?;" (Only filename)

markUpToDate :: State -> FilePath -> IO ()
markUpToDate state@State{..} filename = Sql.withTransaction conn $ do
    Sql.execute conn "UPDATE file SET lifecycle = 'UPTODATE' WHERE filename = ?;" (Only filename)

markBuildFailed :: State -> FilePath -> IO ()
markBuildFailed state@State{..} filename = Sql.withTransaction conn $ do
    Sql.execute conn "UPDATE file SET lifecycle = 'BUILDFAILED' WHERE filename = ?;" (Only filename)


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