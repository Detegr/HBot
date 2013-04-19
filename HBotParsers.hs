module HBotParsers(parsers,MsgType(..),MsgHost,Msg(..)) where

import Text.Parsec
import Text.Parsec.Text

type Channel = String
type MsgData = String
data MsgType = PRIVMSG | PING | Unknown deriving (Show, Eq)

data MsgHost = MsgHost { nickName :: String, userName :: String, hostName :: String } deriving Show
data Msg = Msg
  { msgType    :: MsgType,
    msgHost    :: Maybe MsgHost,
    msgChannel :: Maybe Channel,
    msgData    :: String
  } deriving Show

normalMessage :: Parser Msg
normalMessage = do
  char ':'
  nick <- many . noneOf $ "!"
  char '!'
  user <- many . noneOf $ "@"
  char '@'
  host <- many . noneOf $ " "
  string " PRIVMSG "
  channel <- many . noneOf $ " "
  string " :"
  msgdata <- many . noneOf $ "\r\n"
  return $ Msg PRIVMSG (Just $ MsgHost nick user host) (Just channel) msgdata

pingMessage :: Parser Msg
pingMessage = do
  string "PING "
  char ':'
  to <- many . noneOf $ "\n"
  return $ Msg PING Nothing Nothing to

stupidParser :: Parser Msg
stupidParser = do
  d <- many . noneOf $ "\r\n"
  return $ Msg Unknown Nothing Nothing d

parsers = normalMessage <|> pingMessage <|> stupidParser
