module Plugin.Admin(adminCommand) where

import Config
import Parser
import Control.Monad

configPath = "HBot.conf"

createNew c h = do
  addToConfig c "AdminUsers" (show h) ""
  saveConfig c configPath

isAuthorized c h = do
    au <- getSection c "AdminUsers"
    if length au == 0
      then do
        createNew c h
        isAuthorized c h
      else return $ any authorized (map fst au)
 where authorized x = x == (show h)

adminCommand :: (MsgHost, [String], [String]) -> IO String
adminCommand (host,params,args) = do
  withLoadedConfig configPath $ \c -> do
    ok <- isAuthorized c host
    putStrLn $ show args
    if ok
      then return $ checkCommand args
      else return $ "You're not authorized to execute admin commands!"

checkCommand args =
  case head args of
    "reloadPlugins" -> "reloadPlugins"
    _               -> "No such command"
