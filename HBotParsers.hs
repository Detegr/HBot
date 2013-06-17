{-# LANGUAGE OverloadedStrings #-}

module HBotParsers(parsers,MsgHost,Msg(..)) where

import Text.Parsec
import Text.Parsec.Text

data MsgHost = MsgHost { nickName :: String, userName :: String, hostName :: String } deriving Show
data Msg = Msg
  {
    prefix   :: Either String MsgHost,
    command  :: Either String Integer,
    params   :: [String],
    trailing :: String
  } deriving Show

parseHost :: Parser (Either String MsgHost)
parseHost = do
  char ':'
  nick <- many . noneOf $ "!"
  char '!'
  user <- many . noneOf $ "@"
  char '@'
  host <- many . noneOf $ " "
  return $ Right (MsgHost nick user host)

parseServer :: Parser (Either String MsgHost)
parseServer = do
  char ':'
  rest <- many . noneOf $ " "
  return $ Left rest

parseAnyPrefix :: Parser (Either String MsgHost)
parseAnyPrefix = do
    anystr <- many . noneOf $ " "
    return $ Left anystr

parsePrefix :: Parser (Either String MsgHost)
parsePrefix = try parseHost <|> try parseServer <|> parseAnyPrefix

parseCommand :: Parser (Either String Integer)
parseCommand = do
  num <- try . many $ digit
  str <- many . noneOf $ " "
  if (length num) /= 0 then return $ Right (read num :: Integer) else return $ Left str

normalMessage :: Parser Msg
normalMessage = do
  prefix <- parsePrefix
  char ' '
  command <- parseCommand
  params <- many . noneOf $ ":"
  char ':'
  msgdata <- many . noneOf $ "\r\n"
  return $ Msg prefix command (words params) msgdata

pingMessage :: Parser Msg
pingMessage = do
  string "PING "
  char ':'
  to <- many . noneOf $ "\r\n"
  return $ Msg (Left "") (Left "PING") [] to

parsers = try normalMessage <|> pingMessage
