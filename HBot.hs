{-# LANGUAGE OverloadedStrings #-}

module Main where

import Connection
import Config
import Parser
import Plugin

import qualified Data.ByteString as B
import Data.List
import Text.Parsec (parse)
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.IO as T
import System.IO
import Data.Either.Utils
import Data.Maybe
import Control.Monad.Reader (liftIO, ReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad (guard)
import Control.Monad.State
import PluginData
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)

type HBotState = ([(String, HBotPlugin)], Connection)

say :: Handle -> PluginResult -> IO()
say h rslt = do
          case rslt of
            Command (Messages msgs) to -> do
              mapM_ (\s -> do
                      B.hPutStr h (ircStr $ show (Command (Message s) to))
                      threadDelay 100000) msgs
            _ -> do
              B.hPutStr h (ircStr . show $ rslt)
              putStrLn $ "Sent: " ++ (show rslt)

handlePrivmsg :: MsgHost -> [String] -> String -> StateT HBotState IO()
handlePrivmsg host params trailing = do
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
          loop
        else liftIO $ say (handle c) ret
    _ -> return ()
  where args = Data.List.tail . Data.List.words $ trailing
        cmd  = Data.List.head . Data.List.words $ trailing
        hostnick = nickName host

handleMsg :: Msg -> StateT HBotState IO()
handleMsg (Msg pr c p t) =
  case c of
    Left "PRIVMSG" -> handlePrivmsg (fromRight pr) p t
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
  loop
commandHandler x = return ()

loop :: StateT HBotState IO()
loop = do
  (plugins,c) <- get
  line <- liftIO (try $ B.hGetLine (handle c) :: IO (Either IOError B.ByteString))
  case line of
    Left _ -> do
      newconn <- liftIO $ reconnect c
      put (plugins,c)
    Right str ->
      case decodeUtf8' str of
        Left  _    -> return ()
        Right text -> do
          case parseInput text of
            Left err  -> liftIO $ putStrLn $ show err
            Right val -> do
              liftIO $ putStrLn $ show val
              handleMsg val
  loop
 where parseInput s = parse lineParser "" s

getHBotConf :: IO(Maybe(String, Int, String, String))
getHBotConf = withLoadedConfig "HBot.conf" $ runMaybeT $ do
  server <- lift $ getItem "Server"   (Just "Connection")
  port   <- lift $ getItem "Port"     (Just "Connection")
  nick   <- lift $ getItem "Nick"     (Just "Connection")
  name   <- lift $ getItem "RealName" (Just "Connection")
  let arr=[server,port,nick,name]
  mapM_ (guard . isJust) arr
  mbvalues <- mapM (return . snd . fromJust) arr
  mapM_ (guard . isJust) mbvalues
  return (getValue server, read . getValue $ port, getValue nick, getValue name)
 where getValue = fromJust . snd . fromJust

main :: IO()
main = do
  plugins <- initPlugins
  conf <- getHBotConf
  case conf of
    Just (server,port,nick,name) -> do
      c <- doConnection server port nick name
      runStateT loop (plugins, c)
      return ()
    Nothing -> putStrLn "HBot.conf invalid. No connection information."
