module IRCConnection(mkConnection,doConnection,ircStr) where

import Data.List
import Network.Socket
import Debug.Trace

ircStr :: String -> String
ircStr s = s ++ "\r\n"

connectionStrings :: String -> String -> [String]
connectionStrings n rn = [ircStr $ intercalate rn ["USER "," "," * :",""],
                          ircStr $ "NICK " ++ n]
                                   
debug = flip trace

addrFromStr :: String -> IO HostAddress
addrFromStr addr = getAddrInfo Nothing (Just addr) Nothing >>= \infos ->
                   return $ getAddr (addrAddress . head $ infos)

getAddr :: SockAddr -> HostAddress
getAddr (SockAddrInet _ h) = h

mkConnection :: String -> Int -> IO Socket
mkConnection addr port = socket AF_INET Stream defaultProtocol >>= \sock ->
                         addrFromStr addr >>= \hostaddr ->
                         let sockaddr = (SockAddrInet (fromIntegral port :: PortNumber) hostaddr)
                            in
                            connect sock sockaddr >>
                            putStrLn ("Connecting to " ++ (show sockaddr)) >>
                            return sock `debug` "Connected"

doConnection :: Socket -> String -> String -> IO()
doConnection s nick realname
    = mapM_ (\str -> send s str) $ connectionStrings nick realname
