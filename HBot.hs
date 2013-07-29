{-# LANGUAGE OverloadedStrings #-}

module Main where

import Connection
import Config
import Parser
import Plugin
import Plugin.Admin

import qualified Data.ByteString as B
import Data.List
import Text.Parsec
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.IO as T
import System.IO
import Data.Either.Utils
import Data.Maybe

say :: Handle -> Command String -> IO()
say h s = do
          B.hPutStr h (ircStr . show $ s)
          putStrLn $ "Sent: " ++ (show s)

handlePrivmsg hdl host params trailing plugins nick c =
  case lookup cmd plugins of
    Just p -> putStrLn ("Running plugin " ++ cmd) >>
      usePluginIO p (host, params, args) >>= \ret ->
      if cmd == "!admin" && ret == (PRIVMSG "reloadPlugins" hostnick)
        then reloadPlugins plugins >>= loop c
        else say hdl ret
    _      -> return ()
  where args = Data.List.tail . Data.List.words $ trailing
        cmd  = Data.List.head . Data.List.words $ trailing
        hostnick = nickName host

handleMsg (Msg pr c p t) (Connection a port n r h) plugins
  | pr == Left "PING" = say h $ PONG (fromLeft c)
  | t == "Nickname is already in use." = reconnect (Connection a port (n ++ "_") r h) >>= \c -> loop c plugins
  | c == Left "PRIVMSG" = handlePrivmsg h (fromRight pr) p t plugins n (Connection a port n r h)
  | otherwise = return ()

loop c plugins = do
  str <- B.hGetLine (handle c)
  case decodeUtf8' str of
    Left err  -> loop c plugins
    Right text -> do
      case parseInput text of
        Left err  -> putStrLn $ show err
        Right val -> do
          putStrLn $ show val
          handleMsg val c plugins
      loop c plugins
 where parseInput s = parse lineParser "" s

data ConnectionData = ConnectionData { server :: String, port :: Int, nick :: String, name :: String }

getConnectionInfo s = do
  server <- lookup "Server"   s
  port   <- lookup "Port"     s
  nick   <- lookup "Nick"     s
  name   <- lookup "RealName" s
  return (fromJust server, read (fromJust port), fromJust nick, fromJust name)

main = do
  plugins <- initPlugins
  withLoadedConfig "HBot.conf" $ \conf -> do
    s <- getSection conf "Connection"
    case getConnectionInfo s of
      Just (server,port,nick,name) -> doConnection server port nick name >>= \c -> loop c plugins
      Nothing                      -> putStrLn "HBot.conf invalid. No connection information."
