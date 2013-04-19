{-# LANGUAGE OverloadedStrings #-}

import IRCConnection
import HBotParsers
import Network.Socket
import Data.List
import Text.Parsec
import Text.Parsec.Text
import Data.Text

handleMsg (Msg t h c d) s
  | t == PING = send s (ircStr $ "PONG :" ++ d) >> return ()
  | otherwise = return ()

loop :: Socket -> IO()
loop s = do
         str <- recv s 4096
         putStrLn $ str
         case parse parsers "" (pack str) of
           Left err  -> putStrLn $ show err
           Right val -> handleMsg val s
         loop s

main = mkConnection "irc.quakenet.org" 6667 >>= \c ->
       putStrLn "Made a connection" >>
       doConnection c "HBot" "Haskell bot" >>
       loop c
