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

irc :: Handle -> String -> IO()
irc h s = B.hPutStr h (ircStr s)

handleMsg (Msg t h c d) hdl
  | t == PING = irc hdl ("PONG :" ++ d) >> return ()
  | otherwise = return ()

loop :: Handle -> IO()
loop h = do
         str <- B.hGetLine h
         T.putStrLn $ (decodeUtf8 str)
         case parse parsers "" (decodeUtf8 str) of
           Left err  -> putStrLn $ show err
           Right val -> handleMsg val h
         loop h

main = do
       h <- mkConnection "irc.quakenet.org" 6667
       putStrLn "Made a connection"
       doConnection h "HBot" "Haskell bot"
       loop h
