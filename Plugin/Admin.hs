module Plugin.Admin(adminCommand) where

import Config
import Parser
import Control.Monad
import Connection

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

adminCommand :: (MsgHost, [String], [String]) -> IO (Command String)
adminCommand (host,params,args) = do
  withLoadedConfig configPath $ \c -> do
    ok <- isAuthorized c host
    if ok
      then return $ checkCommand args to
      else return $ PRIVMSG "You're not authorized to execute admin commands!" to
 where to = nickName host

checkCommand [] to = PRIVMSG "Admin plugin" to
checkCommand args to =
  case head args of
    "reloadPlugins" -> PRIVMSG "reloadPlugins" to
    "join"          -> if length args > 1
                         then JOIN (head . tail $ args)
                         else PRIVMSG "!admin join takes one parameter" to
    _               -> PRIVMSG "No such command" to
