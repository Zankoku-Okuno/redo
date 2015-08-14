{-# LANGUAGE LambdaCase #-}
module Distribution.Redo.Util (
      module X, listToMaybe, fromMaybe
    , snoc
    , breakM
    , breakExts
    , apFst, apSnd, apPair
    , spanJoin
    , assocUpdate
    , putErrLn, die
    , removeFileIfExists
    ) where


import Data.Maybe (listToMaybe, fromMaybe)
import Data.List as X

import Control.Applicative as X
import Control.Monad as X

import System.IO (stderr, hPutStrLn)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitFailure)



snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

breakM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
breakM p [] = return ([], [])
breakM p (x:xs) = p x >>= \case
    True -> return ([], x:xs)
    False -> do
        (as, bs) <- breakM p xs
        return (x:as, bs)

breakExts :: String -> [(String, String)]
breakExts filename = map (apPair concat) candidates
    where
    candidates = takeWhile (not . null . snd) $ iterate creep ([basename], extensions)
    (basename, extensions) = apSnd (groupBy (const (/= '.'))) $ spanJoin (/= '.') $ spanJoin (== '.') ("", filename)
    creep (pre, x:post) = (pre `snoc` x, post)

apFst :: (a -> c) -> (a, b) -> (c, b)
apFst f (a, b) = (f a, b)

apSnd :: (b -> c) -> (a, b) -> (a, c)
apSnd f (a, b) = (a, f b)

apPair :: (a -> b) -> (a, a) -> (b, b)
apPair f (a, b) = (f a, f b)

spanJoin :: (a -> Bool) -> ([a], [a]) -> ([a], [a])
spanJoin f (pre, xs) = (pre ++ mv, post)
    where (mv, post) = span f xs

assocUpdate :: (Eq a) => (a, b) -> [(a, b)] -> [(a, b)]
assocUpdate x' [] = [x']
assocUpdate x'@(a', b') (x@(a, b):xs)
    | a == a' = x' : xs
    | otherwise = x : assocUpdate x' xs

putErrLn = hPutStrLn stderr
die msg = putErrLn msg >> exitFailure

removeFileIfExists filepath = do
    exists <- doesFileExist filepath
    when exists $ removeFile filepath