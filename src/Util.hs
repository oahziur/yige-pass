module Util where

import Control.Exception
import Crypto.PBKDF
import System.Directory
import System.IO
import System.Random

import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char

flushStr :: String -> IO ()
flushStr str = do
    putStr str
    hFlush stdout

readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
    case reads s of
        [(x,"")] -> Just x
        _ -> Nothing

genAes :: String -> String -> AES.AES
genAes password iv = AES.initAES keyBS
  where
    keyBS = Char8.pack $ decodeFromStrB16 $ getPassHashB16 password iv

encryptToB16 :: String -> String -> String -> String
encryptToB16 password text iv =
    Char8.unpack $ Base16.encode $ AES.encryptCTR aes ivBS textBS
  where
    aes = genAes password iv
    ivBS = Char8.pack iv
    textBS = Char8.pack text

decryptFromB16 :: String -> String -> String -> String
decryptFromB16 password cipherB16 ivB16 =
    Char8.unpack $ AES.decryptCTR aes ivBS cipherBS
  where
    iv = decodeFromStrB16 ivB16
    ivBS = Char8.pack iv
    aes = genAes password iv
    cipherBS = Char8.pack $ decodeFromStrB16 cipherB16

encodeToStrB16 :: String -> String
encodeToStrB16 str = Char8.unpack $ Base16.encode $ Char8.pack str

decodeFromStrB16 :: String -> String
decodeFromStrB16 str = Char8.unpack $ fst $ Base16.decode $ Char8.pack str

genNewPassword :: Int -> StdGen -> String
genNewPassword len gen =
    take len $ filter isValidChar $ randomRs ('0', 'z') gen
  where
    isValidChar char = Char.isLetter char || Char.isNumber char

genNewSalt :: StdGen -> String
genNewSalt = genNewPassword saltSize
  where
    saltSize = 16

getMasterPassword :: IO String
getMasterPassword = do
    flushStr "master password: "
    pass <- withEcho False getLine
    putChar '\n'
    return pass

getPassHashB16 :: String -> String -> String
getPassHashB16 password salt = sha512PBKDF2 salt password 4096 32

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

updateFile :: FilePath -> String -> IO ()
updateFile fileName newContent = do
    writeFile tempName newContent
    removeFile fileName
    renameFile tempName fileName
  where
    tempName = fileName ++ ".tmp"

attachIndexToEntry :: Integer -> String -> String
attachIndexToEntry num entryStr = "[" ++ show num ++ "]" ++ entryStr
