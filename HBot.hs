{-# LANGUAGE OverloadedStrings #-}

module HBot where

import Connection
import Parser
import Plugin

import Network.Socket hiding(send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import Data.List
import Text.Parsec
import Text.Parsec.Text
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.IO as T
import System.IO
import Data.Either.Utils

irc :: Handle -> String -> IO()
irc h s = B.hPutStr h (ircStr s)

handlePrivmsg host params trailing =
  putStrLn cmd
 where args = Data.List.tail . Data.List.words $ trailing
       cmd  = Data.List.head . Data.List.words $ trailing

handleMsg (Msg pr c p t) (Connection a port n r h)
  | pr == Left "PING" = irc h ("PONG " ++ (fromLeft c)) >> return ()
  | t == "Nickname is already in use." = reconnect (Connection a port (n ++ "_") r h) >>= \c -> loop c
  | c == Left "PRIVMSG" = handlePrivmsg (fromRight pr) p t
  | otherwise = return ()

loop :: Connection -> IO()
loop c = do
         str <- B.hGetLine (handle c)
         T.putStrLn $ (decodeUtf8 str)
         case parse lineParser "" (decodeUtf8 str) of
           Left err  -> putStrLn $ show err
           Right val -> do
                 putStrLn $ show val
                 handleMsg val c
         loop c

main = initPlugins >> doConnection "irc.quakenet.org" 6667 "HBot" "Haskell bot" >>= \c -> loop c
