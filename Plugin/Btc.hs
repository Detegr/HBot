{-# LANGUAGE OverloadedStrings #-}

module Plugin.Btc(btc) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types (Parser)
import Control.Applicative
import qualified Data.Text as T
import Safe (readMay)
import Text.Printf
import PluginData

data BtcData = BtcData
  { high :: Double
  , last :: Double
  , timestamp :: Int
  , bid  :: Double
  , volume  :: Double
  , low  :: Double
  , ask  :: Double
  } deriving Show

data ConversionData = ConversionData
  { sell :: Double
  , buy  :: Double
  } deriving Show

readField :: (Read a) => Object -> T.Text -> Parser a
readField o f = do
  v <- o .: f
  case readMay (T.unpack v) of
    Nothing -> fail $ "Bad field: " ++ T.unpack f
    Just r -> return r

instance FromJSON BtcData where
  parseJSON (Object o) = do
    BtcData <$> readField o "high" <*>
                readField o "last" <*>
                readField o "timestamp" <*>
                readField o "bid" <*>
                readField o "volume" <*>
                readField o "low" <*>
                readField o "ask"

instance FromJSON ConversionData where
  parseJSON (Object o) = do
    ConversionData <$> readField o "sell" <*>
                       readField o "buy"

valueStr :: String
valueStr = "http://www.bitstamp.net/api/ticker/"

conversionStr :: String
conversionStr = "https://www.bitstamp.net/api/eur_usd/"

value :: IO L.ByteString
value = simpleHttp valueStr

conversion :: IO L.ByteString
conversion = simpleHttp conversionStr

parseBtc :: L.ByteString -> Either String BtcData
parseBtc = eitherDecode'

parseRate :: L.ByteString -> Either String ConversionData
parseRate = eitherDecode'

btcStr :: BtcData -> ConversionData -> String
btcStr b c = "High: $" ++ (printf "%.2f" $ high b) ++ (" (" ++ (printf "%.2f" $ high b / buy c) ++ "e)") ++
             ", Low: $" ++ (printf "%.2f" $ low b) ++ (" (" ++ (printf "%.2f" $ low b / buy c) ++ "e)") ++
             ", Last: $" ++ (printf "%.2f" $ Plugin.Btc.last b) ++ (" (" ++ (printf "%.2f" $ Plugin.Btc.last b / buy c) ++ "e)")

btc :: PluginData a -> IO (PluginResult a)
btc pd = do
  btc <- fmap parseBtc value
  rate <- fmap parseRate conversion
  case rate of
    Left err -> msgToChannel pd err
    Right rate ->
      case btc of
        Left err -> msgToChannel pd err
        Right btc -> msgToChannel pd $ btcStr btc rate
