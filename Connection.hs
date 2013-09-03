{-# LANGUAGE FlexibleInstances #-}
module Connection(doConnection,ircStr,Connection(..),reconnect,Command(..),CommandType(..)) where

import Data.List
import Network.Socket hiding(send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as B
import Debug.Trace
import System.IO

data Connection = Connection {address :: String, port :: Int, nick :: String, realname :: String, handle :: Handle}
data CommandType a = Message String | Messages [String] | Pong | Join deriving Eq
data Command t a = Command t String deriving Eq
instance Show (Command (CommandType t) a) where
  show (Command (Message msg) to) = "PRIVMSG " ++ to ++ " :" ++ msg
  show (Command (Messages msgs) to) = foldl' (\a s -> a ++ "PRIVMSG " ++ to ++ " :" ++ s ++ "\r\n") [] msgs
  show (Command Pong a)      = "PONG " ++ a
  show (Command Join a)      = "JOIN " ++ a

ircStr :: String -> UTF8.ByteString
ircStr s = if isPrefixOf "\r\n" s
             then UTF8.fromString s
             else UTF8.fromString (s ++ "\r\n")

privmsg to msg = "PRIVMSG " ++ to ++ " :" ++ msg
join to = "JOIN " ++ to
pong s = "PONG " ++ s

connectionStrings :: String -> String -> [UTF8.ByteString]
connectionStrings n rn = [ircStr $ intercalate rn ["USER "," "," * :",""],
                          ircStr $ "NICK " ++ n]
                                   
debug = flip trace

addrFromStr :: String -> IO HostAddress
addrFromStr addr = getAddrInfo Nothing (Just addr) Nothing >>= \infos ->
                   return $ getAddr (addrAddress . head $ infos)

getAddr :: SockAddr -> HostAddress
getAddr (SockAddrInet _ h) = h

mkConnection :: String -> Int -> IO Handle
mkConnection addr port = socket AF_INET Stream defaultProtocol >>= \sock ->
                         addrFromStr addr >>= \hostaddr ->
                         let sockaddr = (SockAddrInet (fromIntegral port :: PortNumber) hostaddr)
                            in do
                            connect sock sockaddr
                            Prelude.putStrLn ("Connecting to " ++ (show sockaddr))
                            hdl <- socketToHandle sock ReadWriteMode
                            hSetBuffering hdl LineBuffering
                            return hdl `debug` "Connected"

doConnection addr port nick realname = do
    hdl <- mkConnection addr port
    mapM_ (\str -> B.hPutStr hdl str) $ connectionStrings nick realname
    return $ Connection addr port nick realname hdl

reconnect (Connection a p n r h) = do
    hClose h `debug` "Reconnecting..."
    doConnection a p n r
