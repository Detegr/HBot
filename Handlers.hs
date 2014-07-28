module Handlers(msgHandler, HBotState) where

import Connection
import Config
import Parser
import Plugin
import PluginData

import Control.Monad.State
import qualified Data.ByteString as B
import Control.Concurrent (threadDelay)
import System.IO
import Data.Either.Utils
import Data.Maybe (catMaybes)

type HBotState a = ([(String, (Maybe a, HBotPlugin a))], Connection)

say :: Handle -> (PluginResult a) -> IO()
say h rslt =
  case rslt of
    Result (Command (Messages msgs) to) _ -> do
      mapM_ (\s -> do
              B.hPutStr h (ircStr $ show (Command (Message s) to))
              threadDelay 100000) msgs
    NoResult -> return ()
    _ -> do
      B.hPutStr h (ircStr . show . resultCmd $ rslt)
      putStrLn $ "Sent: " ++ (show . resultCmd $ rslt)

privMsgHandler :: MsgHost -> [String] -> String -> StateT (HBotState a) IO()
privMsgHandler host params trailing = do
  (plugins, c) <- get
  case lookup cmd plugins of
    Just (st,p) -> do
      ret <- liftIO $ runPlugin p st
      if cmd == "!admin" && resultCmd ret == Command (Message "reloadPlugins") hostnick
        then do
          newplugins <- liftIO $ reloadPlugins plugins
          put (newplugins, c)
        else do
          let plgs = map (\(pname, pdata) -> if pname == cmd then (pname, (resultState ret, p)) else (pname, pdata)) plugins
          put (plgs, c)
          liftIO $ say (handle c) ret
    _ -> runImplicitPlugin "analyzer" plugins c >> runImplicitPlugin "logger" plugins c
  where args = tail . words $ trailing
        cmd = case words trailing of
                [] -> []
                wt -> case head wt of
                  "analyzer" -> ""
                  "logger"   -> ""
                  cmd        -> cmd
        channel = case params of
                    [] -> []
                    _  -> head params
        hostnick = nickName host
        runPlugin p st = putStrLn ("Running plugin " ++ cmd) >> usePluginIO p (host, params, args, st)
        runImplicitPlugin name plugins c =
          case lookup name plugins of
            Just (st,p)  -> liftIO $ say (handle c) =<< usePluginIO p (host, params, (words trailing), st)
            Nothing -> return ()


msgHandler :: Msg -> StateT (HBotState a) IO()
msgHandler (Msg pr c p t) =
  case c of
    Left "PRIVMSG" -> privMsgHandler (fromRight pr) p t
    Right i        -> commandHandler i
    _ -> case pr of
      Left "PING"  -> pingHandler $ fromLeft c
      _            -> return ()

pingHandler :: String -> StateT (HBotState a) IO()
pingHandler pong = do
  (_,conn) <- get
  liftIO $ say (handle conn) $ Result (Command Pong pong) Nothing

reconnectChangeNick :: StateT (HBotState a) IO()
reconnectChangeNick = do
  (plugins, (Connection a port n r h)) <- get
  newconn <- liftIO $ reconnect (Connection a port (n ++ "_") r h)
  put (plugins, newconn)

doAutoJoins :: StateT (HBotState a) IO()
doAutoJoins = do
  (_, conn) <- get
  liftIO $ withLoadedConfig "HBot.conf" $ do
    keys <- getSectionKeys "AutoJoin"
    liftIO $ mapM_ (Handlers.join $ handle conn) keys

join :: Handle -> String -> IO()
join handle channel = say handle =<< cmd Join channel

commandHandler :: Integer -> StateT (HBotState a) IO()
commandHandler 376 = doAutoJoins -- End of MOTD
commandHandler 433 = reconnectChangeNick -- Nick already in use
commandHandler 437 = reconnectChangeNick -- Nick currently unavailable
commandHandler x = liftIO $ putStrLn $ "UNHANDLED COMMAND: " ++ (show x)
