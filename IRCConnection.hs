module IRCConnection(mkConnection,doConnection,ircStr) where

import Data.List
import Network.Socket hiding(send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as B
import Debug.Trace
import System.IO

ircStr :: String -> UTF8.ByteString
ircStr s = UTF8.fromString (s ++ "\r\n")

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

doConnection :: Handle -> String -> String -> IO()
doConnection h nick realname
    = mapM_ (\str -> B.hPutStr h str) $ connectionStrings nick realname
