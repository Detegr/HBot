module Plugin.Lanit(lanParty) where

import PluginData

import System.Locale (defaultTimeLocale)
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.DateTime (diffMinutes)
import Data.List
import Control.Exception (try, SomeException)
import Data.Maybe

data LanParty = LanParty {whenDate :: String, whenTime :: String, at :: String} deriving (Show,Read)

mkLanParty :: [String] -> LanParty
mkLanParty args = LanParty date time at
 where date = args !! 0
       time = args !! 1
       at   = args !! 2

timeStr :: LanParty -> String
timeStr lp = intercalate " " [whenDate lp, whenTime lp]

parseDate :: TimeZone -> String -> Maybe UTCTime
parseDate tz d = do
  let lt=parseTime defaultTimeLocale "%d.%m.%Y %H:%M" d :: Maybe LocalTime
  case lt of
    Just lt -> Just (localTimeToUTC tz lt)
    Nothing -> Nothing

showLanParty :: LanParty -> IO [String]
showLanParty lp = do
  now <- fmap zonedTimeToUTC getZonedTime
  tz  <- getCurrentTimeZone
  case parseDate tz $ timeStr lp of
    Just _  -> return [intercalate " " ["Lanit", timeStr lp, "@", at lp], daysToNext tz now lp]
    Nothing -> return ["Invalid DateTime"]

saveLanParty :: LanParty -> IO()
saveLanParty lp = writeFile "lanit.txt" $ show lp

nextLanParty :: IO (Maybe LanParty)
nextLanParty = do
  str <- try $ readFile "lanit.txt" :: IO (Either SomeException String)
  return $ case str of
    Left _ -> Nothing
    Right lp ->
      case (reads lp :: [(LanParty, String)]) of
        [(a, "")] -> Just a
        _         -> Nothing

diffStr :: UTCTime -> UTCTime -> String
diffStr a b = do
  let mindiff=diffMinutes a b
  let mins=mindiff `mod` 60
  let hours=mindiff `div` 60 `mod` 24
  let days=mindiff `div` 60 `div` 24
  if days<0 && days > -3
    then "Lanit ovat käynnissä!"
    else if days<0 && days >= 3
      then "Seuraavia laneja ei tiedossa"
      else "Seuraaviin laneihin " ++ (show days) ++ " päivää, " ++ (show hours) ++ " tuntia, " ++ (show mins) ++ " minuuttia!"

daysToNext :: TimeZone -> UTCTime -> LanParty -> String
daysToNext tz now lp = do
  let lp'=fromJust $ parseDate tz $ timeStr lp
  diffStr lp' now

lanParty :: PluginData a -> IO (PluginResult a)
lanParty pd = do
  if length (arguments pd) == 3
    then do
      saveLanParty $ mkLanParty (arguments pd)
      mblp <- nextLanParty
      case mblp of
        Just lp -> do
          showLanParty lp >>= msgsToChannel pd
        Nothing -> msgToChannel pd $ "Seuraavia laneja ei tiedossa."
    else do
      mblp <- nextLanParty
      case mblp of
        Just lp -> showLanParty lp >>= msgsToChannel pd
        Nothing -> msgToChannel pd $ "Seuraavia laneja ei tiedossa."
