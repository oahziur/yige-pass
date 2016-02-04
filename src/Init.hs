module Init (initVault) where

import System.Directory
import System.Random

import Util 
import VaultData

createVault :: String -> IO ()
createVault fileName = do
    putStr "Enter "
    password <- getMasterPassword
    salt <- genNewSalt <$> newStdGen
    let hashB16 = getPassHashB16 password salt
        saltB16 = encodeToStrB16 salt
        v = Vault saltB16 hashB16 []
    putStr "Re-enter "
    password <- getMasterPassword
    if verifyMasterPass password v
        then writeFile fileName $ prettyPrintedStr v
        else putStrLn "Incorrect password!"

initVault :: [String] -> IO ()
initVault [fileName] = do
    existFile <- doesFileExist fileName
    if existFile
        then putStrLn "File exists!"
        else createVault fileName
