module Add (add) where

import System.Hclip (setClipboard)
import System.Random

import Util 
import VaultData

getNewPassword :: IO String
getNewPassword = do
    newPass <- genNewPassword defaultLen <$> newStdGen
    flushStr $ "Password (default: " ++ newPass ++ " ):"
    password <- getLine
    if null password
        then do
            setClipboard newPass
            putStrLn $ newPass ++ "\t\t - copied to clipboard"
            return newPass
        else return password
  where
    defaultLen = 20

getEntryInfo :: Vault -> IO (Maybe VaultEntry)
getEntryInfo vault = do
    putStr "Enter "
    masterPass <- getMasterPassword
    if verifyMasterPass masterPass vault
        then do
            flushStr "Service: "
            service <- getLine
            flushStr "Username: "
            username <- getLine
            password <- getNewPassword
            flushStr "Tags (separate by whitespace): "
            tagsStr <- getLine
            putStrLn "Note: (control-d to finish)"
            note <- getContents
            passIV <- genNewSalt <$> newStdGen
            noteIV <- genNewSalt <$> newStdGen
            let tags = words tagsStr
                passwordB16 = encryptToB16 masterPass password passIV
                noteB16 = encryptToB16 masterPass note noteIV
                passIVB16 = encodeToStrB16 passIV
                noteIVB16 = encodeToStrB16 noteIV
            return $
                Just $
                VaultEntry
                    service
                    username
                    passwordB16
                    passIVB16
                    noteB16
                    noteIVB16
                    tags
        else do
            putStrLn "Incorrect password!"
            return Nothing

addEntry :: Maybe VaultEntry -> Vault -> Vault
addEntry (Just entry) (Vault s m es) = Vault s m $ es ++ [entry]
addEntry Nothing v = v

add :: [String] -> IO ()
add [fileName] = do
    jsonStr <- readFile fileName
    let v = parseVault jsonStr
    newEntry <- getEntryInfo v
    updateFile fileName $ prettyPrintedStr $ addEntry newEntry v
