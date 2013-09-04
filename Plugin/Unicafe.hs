{-# LANGUAGE OverloadedStrings #-}

module Plugin.Unicafe(unicafe) where

import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast.Utf8Only
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Network.HTTP
import Text.Parsec
import Text.Parsec.Text
import Control.Monad
import Data.Dates
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Text.IO (putStrLn)
import Data.List (foldl')
import Data.Char
import Data.Maybe (catMaybes)

import Parser
import Connection
import PluginData

data Restaurant = Metsatalo | Olivia | Porthania | Paarakennus | Rotunda | SocKom | Topelias | Valtiotiede | Ylioppilasaukio |
                  Kookos | Chemicum | Exactum | Physicum | Meilahti | Ruskeasuo | Biokeskus | Korona | Viikuna
                  deriving (Show,Bounded,Eq,Read)

-- This declaration feels very stupid, but quick googling resulted no better alternative...
instance Enum Restaurant where
  toEnum 1  = Metsatalo
  toEnum 2  = Olivia
  toEnum 3  = Porthania
  toEnum 4  = Paarakennus
  toEnum 5  = Rotunda
  toEnum 15 = SocKom
  toEnum 6  = Topelias
  toEnum 7  = Valtiotiede
  toEnum 8  = Ylioppilasaukio
  toEnum 16 = Kookos
  toEnum 10 = Chemicum
  toEnum 11 = Exactum
  toEnum 12 = Physicum
  toEnum 13 = Meilahti
  toEnum 14 = Ruskeasuo
  toEnum 18 = Biokeskus
  toEnum 19 = Korona
  toEnum 21 = Viikuna

  fromEnum Metsatalo       = 1
  fromEnum Olivia          = 2
  fromEnum Porthania       = 3
  fromEnum Paarakennus     = 4
  fromEnum Rotunda         = 5
  fromEnum SocKom          = 15
  fromEnum Topelias        = 6
  fromEnum Valtiotiede     = 7
  fromEnum Ylioppilasaukio = 8
  fromEnum Kookos          = 16
  fromEnum Chemicum        = 10
  fromEnum Exactum         = 11
  fromEnum Physicum        = 12
  fromEnum Meilahti        = 13
  fromEnum Ruskeasuo       = 14
  fromEnum Biokeskus       = 18
  fromEnum Korona          = 19
  fromEnum Viikuna         = 21

-- Gotta learn how to write a Read instance :D
strToRestaurant :: String -> Maybe Restaurant
strToRestaurant s = go $ map toLower s
  where go "metsätalo"       = Just Metsatalo
        go "olivia"          = Just Olivia
        go "porthania"       = Just Porthania
        go "päärakennus"     = Just Paarakennus
        go "rotunda"         = Just Rotunda
        go "sockom"          = Just SocKom
        go "topelias"        = Just Topelias
        go "valtiotiede"     = Just Valtiotiede
        go "ylioppilasaukio" = Just Ylioppilasaukio
        go "kookos"          = Just Kookos
        go "chemicum"        = Just Chemicum
        go "exactum"         = Just Exactum
        go "physicum"        = Just Physicum
        go "meilahti"        = Just Meilahti
        go "ruskeasuo"       = Just Ruskeasuo
        go "biokeskus"       = Just Biokeskus
        go "korona"          = Just Korona
        go "viikuna"         = Just Viikuna
        go _                 = Nothing

unicafeurl :: Int -> Int -> Integer -> Restaurant -> String
unicafeurl w d y id = "http://www.unicafe.fi/lounastyokalu/index.php?option=com_ruokalista&Itemid=29&task=lounaslista_haku&week=" ++
                      show w ++ "&day=" ++ show d ++ "&year=" ++ show y ++ "&rid=" ++ show (fromEnum id) ++ "&lang=1"

source addr = simpleHTTP (getRequest addr) >>= getResponseBody

foodParser :: Parser T.Text
foodParser = do
  f <- many $ noneOf "(["
  try $ do
    many $ noneOf ")"
    char ')'
    return () <|> eof
  t <- try $ string "Maukkaasti" <|> string "Edullisesti" <|> string "Kevyesti"
  return $ T.concat [(T.pack f), (T.pack "- "), (T.pack t)]


joinFoodAndType :: [T.Text] -> [T.Text]
joinFoodAndType [] = []
joinFoodAndType [f] =
  case parse foodParser "" f of
    Left err -> []
    Right f  -> [f]
joinFoodAndType (f:t:xs) =
  case parse foodParser "" (T.concat [f, t]) of
    Left err -> joinFoodAndType xs
    Right f -> [f] ++ joinFoodAndType xs

foodsFromSource :: String -> [String]
foodsFromSource = Prelude.map T.unpack . joinFoodAndType .
                  Prelude.filter ((> 1) . T.length) .
                  Prelude.map fromTagText .
                  Prelude.filter isTagText .
                  dropWhile (not . isTagOpenName "li") .
                  parseTagsT . B.pack

header :: DateTime -> String
header dt = "Food for " ++ (show . day $ dt) ++ "." ++ (show . month $ dt) ++ "." ++ (show . year $ dt)

foodsForRestaurant :: Int -> Int -> Integer -> Restaurant -> IO [String]
foodsForRestaurant w wd y r = fmap foodsFromSource (source $ unicafeurl w wd y r)

spacer :: String
spacer = take 20 $ repeat '-'

restaurantHeader :: Restaurant -> [String] -> [String]
restaurantHeader r f = [spacer, show r ++ ":"] ++ f

restaurantHeaders :: [Restaurant] -> [[String]] -> [[String]]
restaurantHeaders rs fds = map (\(r,f) -> restaurantHeader r f) $ zip rs fds

getRestaurants :: PluginData -> [Restaurant]
getRestaurants pd =
  case catMaybes . map (strToRestaurant) $ arguments pd of
    [] -> [Chemicum, Exactum]
    rs -> rs

unicafe :: PluginData -> IO PluginResult
unicafe pd = do
  dt <- getCurrentDateTime
  let (year, week, weekday) = toWeekDate . dateTimeToDay $ dt
  foods <- mapM (foodsForRestaurant week weekday year) $ getRestaurants pd
  putStrLn $ show . getRestaurants $ pd
  msgsToChannel pd (Data.List.foldl' (\a s -> a ++ s) [header dt] $ restaurantHeaders (getRestaurants pd) foods)
