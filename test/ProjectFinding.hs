import TestUtil

import Distribution.Redo.Sandbox

import System.Directory
import System.FilePath

main = runTests "Project Finding"
    [ Test
        { name = "find `.redo` in same directory (abspath)"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = cwd </> "test/project-finding/project-dir"
                expectedRedoDir = projectDir </> ".redo"
            result <- getSandbox projectDir
            pure $ result `sandboxExpected` Just expectedRedoDir
        }
    , Test
        { name = "find `.redo` in same directory (abspath+slash)"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = cwd </> "test/project-finding/project-dir/"
                expectedRedoDir = projectDir </> ".redo"
            result <- getSandbox projectDir
            pure $ result `sandboxExpected` Just expectedRedoDir
        }
    , Test
        { name = "find `.redo` in same directory (relpath)"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = "test/project-finding/project-dir"
                expectedRedoDir = cwd </> projectDir </> ".redo"
            result <- getSandbox projectDir
            pure $ result `sandboxExpected` Just expectedRedoDir
        }
    , Test
        { name = "find `.redo` in child directory"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = cwd </> "test/project-finding/project-dir"
                subdir = projectDir </> "subdir"
                expectedRedoDir = projectDir </> ".redo"
            sub_result <- getSandbox subdir
            pure $ sub_result `sandboxExpected` Just expectedRedoDir
        }
    , Test
        { name = "find `.redo` in descendant directory"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = cwd </> "test/project-finding/project-dir"
                subsubdir = projectDir </> "subdir" </> "subsubdir"
                expectedRedoDir = projectDir </> ".redo"
            subsub_result <- getSandbox subsubdir
            pure $ subsub_result `sandboxExpected` Just expectedRedoDir
        }
    , Test
        { name = "finds nearest ancestor containing `.redo`"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = cwd </> "test/project-finding/project-dir/subproject"
                expectedRedoDir = projectDir </> ".redo"
            result <- getSandbox projectDir
            pure $ result `sandboxExpected` Just expectedRedoDir
        }
    , Test
        { name = "do not find `.redo` outside of project dir"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = cwd </> "test/project-finding"
            result <- getSandbox projectDir
            pure $ result `sandboxExpected` Nothing
        }
    , Test
        { name = "do not find `.redo` from non-existent directory"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = cwd </> "test/project-finding/project-dir/no-such-thing"
            result <- getSandbox projectDir
            pure $ result `sandboxExpected` Nothing
        }
    , Test
        { name = "do not find `.redo` inside of a `.redo` directory"
        , run = do
            cwd <- getCurrentDirectory
            let projectDir = cwd </> "test/project-finding/project-dir/.redo"
            result <- getSandbox projectDir
            pure $ result `sandboxExpected` Nothing
        }
    -- TODO test through symlinks
    ]

notEqualFilePath a b = not $ equalFilePath a b

sandboxExpected :: Maybe Sandbox -> Maybe FilePath -> TestResult
sandboxExpected Nothing Nothing = Pass
sandboxExpected (Just sandbox) Nothing = Fail $ concat ["redo sandbox unexpectedly found: ", _redoDir sandbox]
sandboxExpected Nothing (Just expectedRedoDir) = Fail $ concat ["could not find directory: expected ", expectedRedoDir]
sandboxExpected (Just sandbox) (Just expectedRedoDir) =
    if _redoDir sandbox `equalFilePath` expectedRedoDir
    then Pass
    else Fail $ concat [_redoDir sandbox, " != ", expectedRedoDir]