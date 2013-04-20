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

handleMsg (Msg t h c d) s
  | t == PING = send s (ircStr $ "PONG :" ++ d) >> return ()
  | otherwise = return ()

loop :: Socket -> IO()
loop s = do
         str <- recv s 4096
         T.putStrLn $ (decodeUtf8 str)
         case parse parsers "" (decodeUtf8 str) of
           Left err  -> putStrLn $ show err
           Right val -> handleMsg val s
         loop s

main = mkConnection "irc.quakenet.org" 6667 >>= \c ->
       putStrLn "Made a connection" >>
       doConnection c "HBot" "Haskell bot" >>
       loop c
