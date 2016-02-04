module Delete (delete) where

import Util
import VaultData

import qualified Data.List as D

getDeletedEntryVault :: VaultEntry -> Vault -> Vault
getDeletedEntryVault e (Vault s m es) = Vault s m $ D.delete e es

deleteEntry :: String -> Maybe VaultEntry -> Vault -> IO()
deleteEntry fileName (Just e) v = do
    flushStr "Selected:\n\n"
    flushStr $ previewEntry e
    password <- getMasterPassword
    if verifyMasterPass password v
        then updateFile fileName $
             prettyPrintedStr $ getDeletedEntryVault e v
        else flushStr "Invalid Password!"
deleteEntry _ Nothing _ = flushStr "Invalid Index!"

delete :: [String] -> IO ()
delete [fileName] = do
    jsonStr <- readFile fileName
    let v = parseVault jsonStr
        es = entries v
    e <- selectEntry es
    deleteEntry fileName e v
