import Prelude hiding (FilePath)
import Data.Maybe
import Control.Monad
import Data.Monoid

import Shelly
import System.Exit
import Text.Regex

import Data.Text (Text)
import qualified Data.Text as T
default(Text)


main :: IO ()
main = do
    results <- forM tests id
    if and results then exitSuccess else exitFailure
    where
    tests = [ test "echo one" echoOne_
            , test "edit one" editOne_
            , test "echo chain" echoChain_
            , test "edit chain" editChain_
            , test "echo diamond" echoDiamond_
            -- TODO test edit the script file
            -- TODO test new script file
            ]

echoOne_ :: Sh Bool
echoOne_ = withRedo ("test" </> "echo") $ do
    cmd "redo-ifchange" "out"
    a <- catIn <$> lastStderr
    cmd "redo-ifchange" "out"
    b <- catIn <$> lastStderr
    pure $ a && not b

editOne_ :: Sh Bool
editOne_ = withRedo ("test" </> "echo") $ do
    cmd "redo-ifchange" "out"
    a <- catIn <$> lastStderr
    saveFile "in" $ do
        writefile "in" "Mesdames, messieurres, bon soir!"
        cmd "redo-ifchange" "out"
    b <- catIn <$> lastStderr
    pure $ a && b

echoChain_ :: Sh Bool
echoChain_ = withRedo ("test" </> "mid") $ do
    cmd "redo-ifchange" "out"
    a <- catIn <$> lastStderr
    cmd "redo-ifchange" "out"
    b <- catIn <$> lastStderr
    pure $ a && not b

editChain_ :: Sh Bool
editChain_ = withRedo ("test" </> "mid") $ do
    cmd "redo-ifchange" "out"
    a <- catIn <$> lastStderr
    saveFile "in" $ do
        writefile "in" "Mesdames, messieurres, bon soir!"
        cmd "redo-ifchange" "out"
    b <- catIn <$> lastStderr
    pure $ a && b

echoDiamond_ :: Sh Bool
echoDiamond_ = withRedo ("test" </> "diamond") $ do
    cmd "redo-ifchange" "out"
    a <- length . filter catIn . T.lines <$> lastStderr
    pure $ a == 1


test :: Text -> Sh Bool -> IO Bool
test testName action = shelly $ do
    echo ""
    echo $ "###### TESTING: " <> testName <> " ######"
    r <- action
    if r
        then echo $ "\x1b[32mOK\x1b[0m: " <> testName
        else echo $ "\x1b[31mFAILURE\x1b[0m: " <> testName
    pure r

withRedo :: FilePath -> Sh a -> Sh a
withRedo dir action = do
    cd dir
    rm_rf ".redo/"
    cmd "redo-init"
    action

catIn :: Text -> Bool
catIn = isJust . matchRegex r . T.unpack
    where
    r = mkRegex "^\\+\\s*cat in"

saveFile :: FilePath -> Sh a -> Sh a
saveFile = saveFiles . (:[])

saveFiles :: [FilePath] -> Sh a -> Sh a
saveFiles files action = do
    originals <- forM files $ \filename -> do
        content <- readfile filename
        pure (filename, content)
    action `finally_sh` (uncurry writefile `mapM_` originals)