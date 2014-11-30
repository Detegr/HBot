{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Plugin.DogeTip where

import PluginData
import Plugin.Util.DogeTipUserDetails
import Network.Bitcoin
import Network.Bitcoin.Wallet
import Data.Text hiding (head, length, map, tail)
import qualified Data.Text.IO as TIO
import Safe (readMay)
import Control.Exception

auth :: Auth
auth = Auth "http://127.0.0.1:22555" user password

(!!!) :: [a] -> Int -> Maybe a
(!!!) xs i = if length xs > i then Just $ xs !! i else Nothing

dogetip :: PluginData a -> IO (PluginResult a)
dogetip pd = do
  if length (arguments pd) > 0 then
      case (head . arguments $ pd) of
        "receive" -> do
          addr <- getNewAddress auth (Just $ pack . getNick $ pd)
          msgToChannel pd $ "plz receive (" ++ getNick pd ++ "): " ++ (unpack addr)
        "send" -> do
          let sendargs = map pack (tail . arguments $ pd)
          if length sendargs < 2
            then msgToChannel pd $ "Usage: !doge send <address> <amount> [comment] [who is the receiver]"
            else
              case readMay $ unpack (sendargs !! 1) of
                Nothing -> msgToChannel pd $ "plz fix amount"
                Just amount -> do
                  mbta <- try $ sendFromAccount auth (pack . getNick $ pd) (sendargs !! 0) amount (sendargs !!! 2) (sendargs !!! 3) :: IO (Either BitcoinException TransactionID)
                  msgToChannel pd $ case mbta of
                    Left err -> "much transaction, no doge"
                    Right ta -> "wow, such doge (" ++ unpack ta ++ ")"
        "balance" -> do
          mbb <- try $ getBalance' auth (pack . getNick $ pd) :: IO (Either BitcoinException BTC)
          msgToChannel pd $ case mbb of
            Left err -> "omg. no doge account. plz '!doge receive' first."
            Right b  -> "such wow, much doge (" ++ getNick pd ++ "): " ++ show b
        _ -> do msgsToNick pd ["much doge. to the moon!", "!doge receive", "!doge send <address> <amount> [comment] [who is the receiver]", "!doge balance"]
  else
    msgsToNick pd ["much doge. to the moon!", "!doge receive", "!doge send <address> <amount> [comment] [who is the receiver]", "!doge balance"]
