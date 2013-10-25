{-# LANGUAGE OverloadedStrings #-}

module Plugin.Arabianranta(arabianranta) where

import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast.Utf8Only
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Network.HTTP
import Text.Parsec
import Text.Parsec.Text
import Data.Dates
import Data.Time.Calendar.WeekDate (toWeekDate)

import PluginData

data FinnishWeekDay = Maanantai|Tiistai|Keskiviikko|Torstai|Perjantai deriving (Enum,Show,Read)

siteAddress :: String
siteAddress = "http://www.compass-group.fi/ravintolat/Helsinki/ravintola-arabianranta/Lounaslista/"

source :: IO String
source = simpleHTTP (getRequest siteAddress) >>= getResponseBody

foodParser :: Parser T.Text
foodParser = do
  spaces
  day <- (try $ string "*")           <|>
         (try $ string "Maanantai")   <|>
         (try $ string "Tiistai")     <|>
         (try $ string "Keskiviikko") <|>
         (try $ string "Torstai")     <|>
         (try $ string "Perjantai")
  txt <- many . noneOf $ "/"
  return $ case length day of
    1 -> T.pack txt
    _ -> T.pack day

parseFood :: T.Text -> String
parseFood f =
  case parse foodParser "" f of
    Left  _ -> ""
    Right pf  -> T.unpack pf

parseTagsFromSource :: String -> [Tag T.Text]
parseTagsFromSource = parseTagsT . B.pack

takeWhileLastTagWithName :: T.Text -> [Tag T.Text] -> [Tag T.Text]
takeWhileLastTagWithName name tags = reverse $ dropWhile (not . isTagCloseName name) (reverse tags)

foodsFromTags :: [Tag T.Text] -> [String]
foodsFromTags tags = filter ((>0) . length) .
                     map parseFood .
                     takeWhile (/= "LOUNAAN HINNAT:") .
                     filter ((>2) . T.length) .
                     map fromTagText .
                     filter isTagText .
                     dropWhile (not . isTagOpenName "table") .
                     takeWhileLastTagWithName "table" $ tags
  
header :: DateTime -> String
header dt = "Food for " ++ (show . day $ dt) ++ "." ++ (show . month $ dt) ++ "." ++ (show . year $ dt)

usage :: String
usage = "Something went wrong"

spacer :: String
spacer = take 20 $ repeat '-'

intToFWeekDayStr :: Int -> String
intToFWeekDayStr i = show $ (toEnum i :: FinnishWeekDay)

dropToDay :: Int -> [String] -> [String]
dropToDay = dropWhile . (/=) . intToFWeekDayStr

takeFromDay :: Int -> [String] -> [String]
takeFromDay = takeWhile . (/=) . intToFWeekDayStr

foodForToday :: Int -> [String] -> [String]
foodForToday d fds
  | d <= 1 = takeFromDay 1 fds
  | d >= 5 = dropToDay 4 fds
  | otherwise = takeFromDay d . dropToDay (d-1) $ fds

arabianranta :: PluginData -> IO PluginResult
arabianranta pd = do
  dt <- getCurrentDateTime
  let (year, week, weekday) = toWeekDate . dateTimeToDay $ dt
  tags <- fmap parseTagsFromSource source
  let todayfds=tail . foodForToday weekday . foodsFromTags $ tags
  case todayfds of
    []  -> msgToChannel pd usage
    fds -> msgsToChannel pd (header dt:spacer:fds)
