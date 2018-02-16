module TestUtil where

import Control.Exception
import System.IO
import System.Exit

data Test = Test
    { run :: IO TestResult
    , name :: String
    }
data TestResult
    = Pass
    | Fail String
    | Error String

runTests :: String -> [Test] -> IO ()
runTests suiteName tests = do
    putErrLn "------------------------------------"
    putErrLn $ concat ["Running ", suiteName, " tests..."]
    results <- mapM go tests
    let fails = filter isFailure results
        errs = filter isError results
    mapM_ (putErrLn . report) fails
    mapM_ (putErrLn . report) errs
    let (tinyReport, exit) = if length fails + length errs == 0
                                then ("OK", exitSuccess)
                                else ("FAILURE", exitFailure)
    putErrLn "------------------------------------"
    putErrLn $ concat [suiteName, ": ", tinyReport]
    putErrLn $ concat ["Ran ", show $ length results, " tests: ", show $ length fails, " failures, ", show $ length errs, " errors"]
    putErrLn "------------------------------------"
    exit
    where
    go :: Test -> IO (String, TestResult)
    go (Test {..}) = do
        r <- run `catch` \(SomeException exn) -> pure $ Error (show exn)
        pure (name, r)
    report (name, Pass) = concat ["Success: ", name]
    report (name, Fail msg) = concat ["FAIL: ", name, ": ", msg]
    report (name, Error msg) = concat ["ERROR: ", name, ": ", msg]
    isFailure (_, Fail _) = True
    isFailure _ = False
    isError (_, Error _) = True
    isError _ = False

putErrLn = hPutStrLn stderr
