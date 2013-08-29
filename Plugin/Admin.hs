module Plugin.Admin(adminCommand) where

import Config
import Parser
import Control.Monad
import Connection
import PluginData

configPath = "HBot.conf"

isAuthorized h = do
    au <- getSection "AdminUsers"
    case au of
      Nothing -> do
        addItem "AdminUsers" (show h) ""
        isAuthorized h
      Just au -> return $ any authorized (map fst $ sectionItems au)
 where authorized x = x == (show h)

adminCommand :: PluginData -> IO PluginResult
adminCommand pd = do
  ok <- withLoadedConfig configPath $ isAuthorized (host pd)
  if ok
    then checkCommand (arguments pd) pd
    else msgToNick pd "You're not authorized to execute admin commands!"

checkCommand [] pd = msgToNick pd "Admin plugin"
checkCommand args pd =
  case head args of
    "reloadPlugins" -> msgToNick pd "reloadPlugins"
    "join"          -> if length args > 1
                         then cmd Join (head . tail $ args)
                         else msgToNick pd "!admin join takes one parameter"
    _               -> msgToNick pd "No such command"
