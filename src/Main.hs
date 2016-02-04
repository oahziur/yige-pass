import System.Environment

import Add
import Delete
import Init
import NewPass
import View

dispatch :: [(String, [String] -> IO ())]
dispatch =
    [ ("--i", initVault)
    , ("--init", initVault)
    , ("--n", newPass)
    , ("--new-pass", newPass)
    , ("--v", view)
    , ("--view", view)
    , ("--a", add)
    , ("--add", add)
    , ("--d", delete)
    , ("--delete", delete)] 
 
main :: IO ()
main = do
    (command:argList) <- getArgs
    let (Just action) = lookup command dispatch
    action argList
