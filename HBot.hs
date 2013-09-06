{-# LANGUAGE OverloadedStrings #-}

module Main where

import Connection
import Config
import Parser
import Plugin
import Handlers

import Text.Parsec (parse)
import Data.Text.Encoding
import qualified Data.ByteString as B
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Exception (try)

loop :: StateT HBotState IO()
loop = do
  (plugins,c) <- get
  line <- liftIO (try $ B.hGetLine (handle c) :: IO (Either IOError B.ByteString))
  case line of
    Left _ -> do
      newconn <- liftIO $ reconnect c
      put (plugins,newconn)
    Right str ->
      case decodeUtf8' str of
        Left  _    -> return ()
        Right text -> do
          case parseInput text of
            Left err  -> liftIO $ putStrLn $ show err
            Right val -> do
              liftIO $ putStrLn $ show val
              msgHandler val
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
