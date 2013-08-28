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
import Control.Monad.Reader (liftIO, ReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad (guard)

say :: Handle -> Command String -> IO()
say h s = do
          B.hPutStr h (ircStr . show $ s)
          putStrLn $ "Sent: " ++ (show s)

handlePrivmsg :: Handle -> MsgHost -> [String] -> String -> [(String, HBotPlugin)] -> String -> Connection -> IO()
handlePrivmsg hdl host params trailing plugins nick c =
  case lookup cmd plugins of
    Just p -> putStrLn ("Running plugin " ++ cmd) >>
      usePluginIO p (host, params, args) >>= \ret ->
      if cmd == "!admin" && ret == (PRIVMSG "reloadPlugins" hostnick)
        then reloadPlugins plugins >>= flip loop c
        else say hdl ret
    _      -> return ()
  where args = Data.List.tail . Data.List.words $ trailing
        cmd  = Data.List.head . Data.List.words $ trailing
        hostnick = nickName host

handleMsg :: Msg -> Connection -> [(String, HBotPlugin)] -> IO()
handleMsg (Msg pr c p t) (Connection a port n r h) plugins
  | pr == Left "PING" = say h $ PONG (fromLeft c)
  | t == "Nickname is already in use." = reconnect (Connection a port (n ++ "_") r h) >>= loop plugins
  | c == Left "PRIVMSG" = handlePrivmsg h (fromRight pr) p t plugins n (Connection a port n r h)
  | otherwise = return ()

loop :: [(String, HBotPlugin)] -> Connection -> IO()
loop plugins c = do
  str <- B.hGetLine (handle c)
  case decodeUtf8' str of
    Left  _    -> loop plugins c
    Right text -> do
      case parseInput text of
        Left err  -> putStrLn $ show err
        Right val -> do
          putStrLn $ show val
          handleMsg val c plugins
      loop plugins c
 where parseInput s = parse lineParser "" s

getHBotConf :: IO(Maybe(String, Int, String, String))
getHBotConf = withLoadedConfig "HBot.conf" $ runMaybeT $ do
  let jc=Just "Connection"
  server <- lift $ getItem "Server"   jc
  port   <- lift $ getItem "Port"     jc
  nick   <- lift $ getItem "Nick"     jc
  name   <- lift $ getItem "RealName" jc
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
    Just (server,port,nick,name)  -> doConnection server port nick name >>= loop plugins
    Nothing                       -> putStrLn "HBot.conf invalid. No connection information."
