import Network.Socket
import Data.Word
import Data.List
import Debug.Trace
import Data.Binary

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

loop :: Socket -> IO()
loop s = recv s 4096 >>= \str ->
         putStrLn str >>
         parseMessage s str >>
         loop s

parseMessage :: Socket -> String -> IO()
parseMessage s str
    | isPrefixOf "PING" str = send s (ircStr (map (\x -> if x=='I' then 'O' else x) str)) >> return ()
    | otherwise = return ()

main = mkConnection "irc.quakenet.org" 6667 >>= \c ->
       putStrLn "Made a connection" >>
       doConnection c "HBot" "Haskell bot" >>
       loop c
