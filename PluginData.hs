module PluginData where

import Parser (MsgHost(..))
import Connection

data PluginResult a = Result { resultCmd :: (Command (CommandType String) String), resultState :: Maybe a } | NoResult
type PluginData a = (MsgHost, [String], [String], Maybe a)

getChannel :: PluginData a -> String
getChannel (_,p,_,_) = head p

getNick :: PluginData a -> String
getNick (h,_,_,_) = nickName h

msgTo :: String -> String -> Maybe a -> IO (PluginResult a)
msgTo to f st = return Result { resultCmd=Command (Message f) to, resultState=st }

msgsTo :: String -> [String] -> Maybe a -> IO (PluginResult a)
msgsTo to f st = return Result { resultCmd=Command (Messages f) to, resultState=st }

msgToNick :: PluginData a -> String -> IO (PluginResult a)
msgToNick pd f = msgTo (getNick pd) f Nothing

msgsToNick :: PluginData a -> [String] -> IO (PluginResult a)
msgsToNick pd f = msgsTo (getNick pd) f Nothing

msgToChannel :: PluginData a -> String -> IO (PluginResult a)
msgToChannel pd f = msgTo (getChannel pd) f Nothing

msgToChannel' :: PluginData a -> String -> Maybe a -> IO (PluginResult a)
msgToChannel' pd f st = msgTo (getChannel pd) f st

msgsToChannel :: PluginData a -> [String] -> IO (PluginResult a)
msgsToChannel pd f = msgsTo (getChannel pd) f Nothing

cmd :: CommandType String -> String -> IO (PluginResult a)
cmd ctype to = return Result { resultCmd=Command ctype to, resultState=Nothing }

arguments :: PluginData a -> [String]
arguments (_,_,a,_) = a

host :: PluginData a -> MsgHost
host (h,_,_,_) = h

params :: PluginData a -> [String]
params (_,p,_,_) = p

state :: PluginData a -> Maybe a
state (_,_,_,st) = st
