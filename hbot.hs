import IRCConnection
import Network.Socket
import Data.List

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
