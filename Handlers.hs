module Handlers(msgHandler, HBotState) where

import Connection
import Parser
import Plugin
import PluginData
import UrlAnalyzer

import Control.Monad.State
import qualified Data.ByteString as B
import Control.Concurrent (threadDelay)
import System.IO
import Data.Either.Utils
import Data.Maybe (catMaybes)
import Network.URI
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf)

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

analyzeUrls :: String -> [String] -> StateT HBotState IO()
analyzeUrls chnl strs = do
  liftIO $ putStrLn $ "Analyzer: " ++ chnl ++ " " ++ (show strs)
  let urls=catMaybes . map analyze $ strs
  urls' <- liftIO $ (try (mapM getTitle urls) :: IO (Either SomeException [String]))
  case urls' of
    Left _ -> return ()
    Right urls'' -> do
      (_,c) <- get
      liftIO $ say (handle c) (Command (Messages urls'') chnl)
 where analyze s =
         case parseURI s of
           Just u  -> if uriScheme u == "http:" || uriScheme u == "https:"
                        then Just s
                        else Nothing
           Nothing -> if isPrefixOf "www." s
                        then Just $ "http://" ++ s
                        else Nothing

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
    _ -> analyzeUrls channel (words trailing)
  where args = tail . words $ trailing
        cmd  = head . words $ trailing
        channel = head params
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

reconnectChangeNick :: StateT HBotState IO()
reconnectChangeNick = do
  (plugins, (Connection a port n r h)) <- get
  newconn <- liftIO $ reconnect (Connection a port (n ++ "_") r h)
  put (plugins, newconn)

commandHandler :: Integer -> StateT HBotState IO()
commandHandler 433 = reconnectChangeNick -- Nick already in use
commandHandler 437 = reconnectChangeNick -- Nick currently unavailable
commandHandler x = liftIO $ putStrLn $ "UNHANDLED COMMAND: " ++ (show x)
