{-# LANGUAGE OverloadedStrings #-}

module Parser(lineParser,MsgHost(..),Msg(..)) where

import Text.Parsec
import Text.Parsec.Text

data MsgHost = MsgHost { nickName :: String, userName :: String, hostName :: String } deriving (Show, Eq)
data Msg = Msg
  {
    prefix   :: Either String MsgHost,
    command  :: Either String Integer,
    params   :: [String],
    trailing :: String
  } deriving Show

ircstring = many . noneOf $ "\r\n" :: Parser String

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
  str <- many . noneOf $ " \r\n"
  if (length num) /= 0 then return $ Right (read num :: Integer) else return $ Left str

lineParser :: Parser Msg
lineParser = do
  prefix <- parsePrefix
  space
  command <- parseCommand
  params <- many . noneOf $ ":"
  skipMany . char $ ':'
  msgdata <- ircstring
  return $ Msg prefix command (words params) msgdata
