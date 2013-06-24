{-# LANGUAGE OverloadedStrings #-}

module Main where

import Connection
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
import Control.Monad.Maybe

say :: Handle -> String -> IO()
say h s = B.hPutStr h (ircStr s)

pluginReturn (hdl, hostnick, ownnick, fstp) ret
  | fstp == ownnick = say hdl $ privmsg hostnick ret
  | otherwise       = say hdl $ privmsg fstp ret

handlePrivmsg hdl host params trailing plugins nick c =
  case lookup cmd plugins of
    Just p -> usePluginIO p (host, params, args) >>= \ret ->
      if cmd == "!admin" && ret == "reloadPlugins"
        then reloadPlugins plugins >>= \(plugs, pdata) -> mapM_ (pluginReturn pluginargs . show) plugs >> loop c pdata
        else pluginReturn pluginargs ret
    _      -> return ()
  where args = Data.List.tail . Data.List.words $ trailing
        cmd  = Data.List.head . Data.List.words $ trailing
        fstp = Data.List.head params
        hostnick = nickName host
        pluginargs = (hdl, hostnick, nick, fstp)

handleMsg (Msg pr c p t) (Connection a port n r h) plugins
  | pr == Left "PING" = say h . pong $ fromLeft c
  | t == "Nickname is already in use." = reconnect (Connection a port (n ++ "_") r h) >>= \c -> loop c plugins
  | c == Left "PRIVMSG" = handlePrivmsg h (fromRight pr) p t plugins n (Connection a port n r h)
  | otherwise = return ()

loop c plugins = do
  str <- B.hGetLine (handle c)
  T.putStrLn $ decodeUtf8 str
  case parseInput str of
    Left err  -> putStrLn $ show err
    Right val -> do
      putStrLn $ show val
      handleMsg val c plugins
  loop c plugins
 where parseInput s = parse lineParser "" $ decodeUtf8 s

data ConnectionData = ConnectionData { server :: String, port :: Int, nick :: String, name :: String }

connect :: ConfigSection -> MaybeT ConnectionData
connect s = do
  server <- lookup "Server" s

main = do
  plugins <- initPlugins
  withLoadedConfig "HBot.conf" $ \conf -> do
    s <- getSection conf "Connection"
    connection <- doConnection "irc.quakenet.org" 6667 "HBot" "Haskell bot"
    loop connection plugins
