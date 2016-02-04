module NewPass (newPass) where

import System.Hclip (setClipboard)
import System.Random

import Util

newPass :: [String] -> IO ()
newPass len = do
    password <-
        genNewPassword (getPassLen $ readMaybe $ concat len) <$> newStdGen
    putStr password
    setClipboard password
    putStrLn "\t\t - copied to clipboard"
  where
    getPassLen (Just len) = len
    getPassLen Nothing = 20
