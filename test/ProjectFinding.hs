import TestUtil

import Distribution.Redo.Sandbox

import System.Directory
import System.FilePath

main = runTests "Project Finding"
    [ Test
        { name = "find `.redo` in same directory"
        , run = do
            cwd <- getCurrentDirectory
            let projectDirRel = "test/project-finding/project-dir"
                projectDirAbs = cwd </> projectDirRel
                projectDirSlash = projectDirAbs ++ "/"
                expectedRedoDir = projectDirAbs </> ".redo"
            abs_result <- _redoDir <$> getSandbox projectDirAbs
            rel_result <- _redoDir <$> getSandbox projectDirRel
            slash_result <- _redoDir <$> getSandbox projectDirSlash
            pure $ if
                    | abs_result `notEqualFilePath` expectedRedoDir -> Fail $ concat [abs_result, " != ", expectedRedoDir]
                    | rel_result `notEqualFilePath` expectedRedoDir -> Fail $ concat [rel_result, " != ", expectedRedoDir]
                    | slash_result `notEqualFilePath` expectedRedoDir -> Fail $ concat [slash_result, " != ", expectedRedoDir]
                    | otherwise -> Pass
        }
    ]

notEqualFilePath a b = not $ equalFilePath a b