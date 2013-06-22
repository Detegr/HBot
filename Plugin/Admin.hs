module Admin(adminCommand) where

import Config
import Parser

configPath = "HBot.conf"

isAuthorized fullhost = do
  withLoadedConfig configPath $ \c -> do
    withSection c "AdminUsers" $ \(_, au) -> do
      any authorized (map snd au)
 where authorized u = u == fullhost

adminCommand :: (MsgHost, [String], String) -> IO String
adminCommand ((MsgHost n u h),params,trailing) = do
  authorized <- isAuthorized fullhost
  case authorized of
    Just x -> return "Authorized user!"
    _      -> return "You're not authorized for admin commands!"
 where fullhost = n ++ u ++ h
