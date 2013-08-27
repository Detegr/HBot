module Plugin.Admin(adminCommand) where

import Config
import Parser
import Control.Monad
import Connection

configPath = "HBot.conf"


isAuthorized h = do
    au <- getSection "AdminUsers"
    case au of
      Nothing -> do
        addItem "AdminUsers" (show h) ""
        isAuthorized h
      Just au -> return $ any authorized (map fst $ sectionItems au)
 where authorized x = x == (show h)

adminCommand :: (MsgHost, [String], [String]) -> IO (Command String)
adminCommand (host,params,args) = do
  withLoadedConfig configPath $ do
    ok <- isAuthorized host
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
