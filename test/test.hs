import Prelude hiding (FilePath)
import Data.Maybe
import Control.Monad

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
    tests = [ echo_
            , diamond_
            ]

echo_ :: IO Bool
echo_ = withRedo ("test" </> "echo") $ do
    results <- cmd "redo-ifchange" "out"
    a <- catIn <$> lastStderr
    results <- cmd "redo-ifchange" "out"
    b <- catIn <$> lastStderr
    pure $ a && not b

diamond_ :: IO Bool
diamond_ = withRedo ("test" </> "diamond") $ do
    results <- cmd "redo-ifchange" "out"
    a <- length . filter catIn . T.lines <$> lastStderr
    pure $ a == 1


withRedo :: FilePath -> Sh a -> IO a
withRedo dir action = shelly $ do
    cd dir
    rm_rf ".redo/"
    cmd "redo-init"
    action

catIn :: Text -> Bool
catIn = isJust . matchRegex r . T.unpack
    where
    r = mkRegex "^\\+\\s*cat in"