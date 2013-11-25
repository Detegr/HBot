module Plugin.Admin(adminCommand) where

import Config
import Parser
import Connection
import PluginData

configPath :: String
configPath = "HBot.conf"

isAuthorized :: MsgHost -> ConfigM Bool
isAuthorized h = do
    mbau <- getSection "AdminUsers"
    case mbau of
      Nothing -> do
        addItem "AdminUsers" (show h) ""
        isAuthorized h
      Just au -> return $ any authorized (map fst $ sectionItems au)
 where authorized x = x == (show h)

adminCommand :: PluginData a -> IO (PluginResult a)
adminCommand pd = do
  ok <- withLoadedConfig configPath $ isAuthorized (host pd)
  if ok
    then checkCommand (arguments pd) pd
    else msgToNick pd "You're not authorized to execute admin commands!"

checkCommand :: [String] -> PluginData a -> IO (PluginResult a)
checkCommand [] pd = msgToNick pd "Admin plugin"
checkCommand args pd =
  case head args of
    "reloadPlugins" -> msgToNick pd "reloadPlugins"
    "join"          -> if length args > 1
                         then cmd Join (head . tail $ args)
                         else msgToNick pd "!admin join takes one parameter"
    _               -> msgToNick pd "No such command"
