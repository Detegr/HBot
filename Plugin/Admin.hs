module Admin(adminCommand) where

import Config
import Parser
import Control.Monad
import Connection

configPath = "HBot.conf"

createNew c h = do
  putStrLn $ "Creating a new admin user " ++ (show h) ++ " to " ++ configPath
  addToConfig c "AdminUsers" (show h) ""
  saveConfig c configPath

isAuthorized c h = do
    au <- getSection c "AdminUsers"
    if length au == 0
      then do
        createNew c h
        isAuthorized c h
      else do
        putStrLn $ show (map fst au)
        return $ any authorized (map fst au)
 where authorized x = x == (show h)

adminCommand :: (MsgHost, [String], String) -> IO String
adminCommand (host,params,trailing) = do
  withLoadedConfig configPath $ \c -> do
    ok <- isAuthorized c host
    if ok
      then return $ "User authorized."
      else return $ "You're not authorized to execute admin commands!"

checkCommand args =
  case head args of
    "reloadPlugins" -> "reloadPlugins"
    _               -> PRIVMSG "No such command"
