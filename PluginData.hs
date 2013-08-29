module PluginData where

import Parser (MsgHost(..))
import Connection(Command)
import System.Plugins.Hotswap
import Control.Monad.Reader
import Connection

type PluginResult = (Command (CommandType String) String)
type PluginData = (MsgHost, [String], [String])
type HBotPlugin = Plugin ((MsgHost, [String], [String]) -> IO PluginResult)

getChannel :: PluginData -> String
getChannel (_,p,_) = head p

getNick :: PluginData -> String
getNick (h,_,_) = nickName h

msgTo :: String -> String -> IO PluginResult
msgTo to f = return $ Command (Message f) to

msgToNick :: PluginData -> String -> IO PluginResult
msgToNick pd f = msgTo (getNick pd) f

msgToChannel :: PluginData -> String -> IO PluginResult
msgToChannel pd f = msgTo (getChannel pd) f

cmd :: CommandType String -> String -> IO PluginResult
cmd ctype to = return $ Command ctype to

arguments :: PluginData -> [String]
arguments (_,_,a) = a

host :: PluginData -> MsgHost
host (h,_,_) = h

params :: PluginData -> [String]
params (_,p,_) = p
