{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Plugin.Give(give) where

import Prelude hiding (catch)
import Config
import PluginData
import Database.PostgreSQL.Simple
import Control.Exception
import qualified Data.ByteString.Char8 as B8
import Data.Either.Utils (fromEither)

getConnectInfo :: ConnectInfo
getConnectInfo = defaultConnectInfo { connectDatabase="loggerdb" }

add :: String -> String -> IO (Either String ())
add key val = do
  c <- connect getConnectInfo
  ok <- try $ add' c
  case ok of
    Left (_ :: SqlError) -> update key val c
    Right _ -> return $ Right ()
 where add' c = do
         execute c "insert into give_plugin (key, value) values (?,?)" [key, val]
         return $ Right ()

update :: String -> String -> Connection -> IO (Either String ())
update key val c = do
  ok <- try $ execute c "update give_plugin SET value=? where key=?" [val, key]
  case ok of
    Left (e :: SqlError) -> return $ Left (B8.unpack $ sqlErrorMsg e)
    Right _ -> return $ Right ()

getVal :: String -> IO (Either String String)
getVal key = do
  mbc <- try $ connect getConnectInfo
  case mbc of
    Left (_ :: SomeException) -> return $ Left "Connection to database failed."
    Right c -> do
      val <- try $ (query c "select value from give_plugin where key=?" (Only key) :: IO [Only String])
      case val of
        Left (e :: SqlError) -> return $ Left (B8.unpack $ sqlErrorMsg e)
        Right [] -> return $ Left $ "Not found: " ++ key
        Right [Only v] -> return $ Right v

list :: IO (Either String [String])
list = do
  mbc <- try $ connect getConnectInfo
  case mbc of
    Left (_ :: SomeException) -> return $ Left "Connection to database failed."
    Right c -> do
      values <- try (query_ c "select key from give_plugin" :: IO [Only String])
      case values of
        Left (e :: SqlError) -> return $ Left (B8.unpack $ sqlErrorMsg e)
        Right xs -> return $ Right $ map (\(Only s) -> s) xs

give :: PluginData a -> IO (PluginResult a)
give pd =
  case arguments pd of
    (key:val:xs) -> add key val >> msgToChannel pd (key ++ " added.")
    (key:xs) -> getVal key >>= msgToChannel pd . fromEither
    _ -> do
      values <- list
      case values of
        Left e -> msgToNick pd e
        Right vals -> msgsToNick pd ("Following keys are present:" : vals)
