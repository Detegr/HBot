{-# LANGUAGE OverloadedStrings #-}

import IRCConnection
import HBotParsers
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
import Control.Monad

irc :: Handle -> String -> IO()
irc h s = B.hPutStr h (ircStr s)

handleMsg (Msg pr c p t) (Connection a port n r h)
  | c == (Left "PING") = irc h ("PONG :" ++ t) >> return ()
  | t == "Nickname is already in use." = reconnect (Connection a port (n ++ "_") r h) >>= \c -> loop c
  | otherwise = return ()

loop :: Connection -> IO()
loop c = do
         str <- B.hGetLine (handle c)
         T.putStrLn $ (decodeUtf8 str)
         case parse parsers "" (decodeUtf8 str) of
           Left err  -> putStrLn $ show err
           Right val -> do
                 putStrLn $ show val
                 handleMsg val c
         loop c

main = do
       doConnection "irc.quakenet.org" 6667 "HBot" "Haskell bot" >>= \c -> loop c
