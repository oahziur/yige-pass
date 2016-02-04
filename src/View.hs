module View (view) where

import System.Hclip (setClipboard)

import Util 
import VaultData

tagPred :: String -> String -> Bool
tagPred tag =
    if tag == ""
        then const True
        else (== tag)

filterTag :: [VaultEntry] -> String -> [VaultEntry]
filterTag entries tag =
    let anyPred = tagPred tag
    in filter (any anyPred . tags) entries

view :: [String] -> IO ()
view [fileName] = view [fileName, ""]
view [fileName,tag] = do
    jsonStr <- readFile fileName
    let v = parseVault jsonStr
        es = filterTag (entries v) tag
    e <- selectEntry es
    password <- getMasterPassword
    if verifyMasterPass password v
        then do
            putStrLn $ showEntry password e
            setClipboard $ getEntryPassword password e
            putStrLn "\nEntry password copied to clipboard."
        else putStrLn "Incorrect password!"
