module PluginData where

import Parser (MsgHost(..))
import Connection

type PluginResult = (Command (CommandType String) String)
type PluginData = (MsgHost, [String], [String])

getChannel :: PluginData -> String
getChannel (_,p,_) = head p

getNick :: PluginData -> String
getNick (h,_,_) = nickName h

msgTo :: String -> String -> IO PluginResult
msgTo to f = return $ Command (Message f) to

msgsTo :: String -> [String] -> IO PluginResult
msgsTo to f = return $ Command (Messages f) to

msgToNick :: PluginData -> String -> IO PluginResult
msgToNick pd f = msgTo (getNick pd) f

msgsToNick :: PluginData -> [String] -> IO PluginResult
msgsToNick pd f = msgsTo (getNick pd) f

msgToChannel :: PluginData -> String -> IO PluginResult
msgToChannel pd f = msgTo (getChannel pd) f

msgsToChannel :: PluginData -> [String] -> IO PluginResult
msgsToChannel pd f = msgsTo (getChannel pd) f

cmd :: CommandType String -> String -> IO PluginResult
cmd ctype to = return $ Command ctype to

arguments :: PluginData -> [String]
arguments (_,_,a) = a

host :: PluginData -> MsgHost
host (h,_,_) = h

params :: PluginData -> [String]
params (_,p,_) = p
