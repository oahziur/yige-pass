{-# LANGUAGE DeriveDataTypeable #-}

module VaultData where

import Text.JSON
import Text.JSON.Generic
import Text.JSON.Pretty
import Util

data VaultEntry = VaultEntry {
  service :: String,
  username :: String,
  passwordB16 :: String,
  passwordIVB16 :: String,
  noteB16 :: String,
  noteIVB16 :: String,
  tags :: [String]
} deriving (Eq, Show, Data, Typeable)

data Vault = Vault {
  saltB16 :: String,
  masterHashB16 :: String,
  entries :: [VaultEntry]
} deriving (Eq, Show, Data, Typeable)
          
prettyPrintedStr :: Data a => a -> String
prettyPrintedStr = render . pp_value . toJSON

parseVault :: String -> Vault
parseVault = decodeJSON

getEntryByIndex ::  [VaultEntry] -> Int -> Maybe VaultEntry
getEntryByIndex es index
  | length es > index = Just $ head $ drop index es
  | otherwise = Nothing

getEntryPassword :: String -> Maybe VaultEntry -> String
getEntryPassword masterPass (Just (VaultEntry _ _ pB16 pIVB16 _ _ _)) =
    decryptFromB16 masterPass pB16 pIVB16
getEntryPassword _ Nothing = ""

verifyMasterPass :: String -> Vault -> Bool
verifyMasterPass password v = masterHashB16 v == getPassHashB16 password saltV
  where
    saltV = decodeFromStrB16 $ saltB16 v

previewEntry :: VaultEntry -> String
previewEntry (VaultEntry service username _ _ _ _ tags) =
    "\t domain:\t" ++
    service ++
    "\n\t username:\t" ++ username ++ "\n\t tags:\t\t" ++ show tags ++ "\n"

previewEntries :: [VaultEntry] -> [String]
previewEntries entries =
    zipWith attachIndexToEntry [0 ..] $ map previewEntry entries

showEntry :: String -> Maybe VaultEntry -> String
showEntry masterPass v@(Just (VaultEntry s u pB16 pIVB16 nB16 nIVB16 _)) =
    "\nservice:\t" ++
    s ++
    "\nusername:\t" ++
    u ++
    "\npassword:\t" ++
    getEntryPassword masterPass v ++
    "\nnote:\n" ++ decryptFromB16 masterPass nB16 nIVB16
showEntry masterPass Nothing = "Invalid index!"

selectEntry :: [VaultEntry] -> IO (Maybe VaultEntry)
selectEntry es = do
    mapM_ putStrLn $ previewEntries es
    flushStr "Select index: "
    selectedIndex <- getLine
    return $ readMaybe selectedIndex >>= getEntryByIndex es
