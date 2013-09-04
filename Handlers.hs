module Handlers(msgHandler, HBotState) where

import Connection
import Config
import Parser
import Plugin
import PluginData

import Control.Monad.Reader (liftIO, ReaderT)
import Control.Monad.State
import qualified Data.ByteString as B
import Control.Concurrent (threadDelay)
import System.IO
import Data.List
import Data.Either.Utils

type HBotState = ([(String, HBotPlugin)], Connection)

say :: Handle -> PluginResult -> IO()
say h rslt =
  case rslt of
    Command (Messages msgs) to -> do
      mapM_ (\s -> do
              B.hPutStr h (ircStr $ show (Command (Message s) to))
              threadDelay 100000) msgs
    _ -> do
      B.hPutStr h (ircStr . show $ rslt)
      putStrLn $ "Sent: " ++ (show rslt)

privMsgHandler :: MsgHost -> [String] -> String -> StateT HBotState IO()
privMsgHandler host params trailing = do
  (plugins, c) <- get
  case lookup cmd plugins of
    Just p -> do
      ret <- liftIO $ do
        putStrLn ("Running plugin " ++ cmd)
        usePluginIO p (host, params, args)
      if cmd == "!admin" && ret == (Command (Message "reloadPlugins") hostnick)
        then do
          newplugins <- liftIO $ reloadPlugins plugins
          put (newplugins, c)
        else liftIO $ say (handle c) ret
    _ -> return ()
  where args = tail . words $ trailing
        cmd  = head . words $ trailing
        hostnick = nickName host

msgHandler :: Msg -> StateT HBotState IO()
msgHandler (Msg pr c p t) =
  case c of
    Left "PRIVMSG" -> privMsgHandler (fromRight pr) p t
    Right i        -> commandHandler i
    _ -> case pr of
      Left "PING"  -> pingHandler $ fromLeft c
      _            -> return ()

pingHandler :: String -> StateT HBotState IO()
pingHandler pong = do
    (_,conn) <- get
    liftIO $ say (handle conn) $ Command Pong pong

commandHandler :: Integer -> StateT HBotState IO()
commandHandler 443 = do
  (plugins, (Connection a port n r h)) <- get
  newconn <- liftIO $ reconnect (Connection a port (n ++ "_") r h)
  put (plugins, newconn)
commandHandler x = return ()
