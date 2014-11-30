{-# LANGUAGE TemplateHaskell,OverloadedStrings,ScopedTypeVariables #-}

module Plugin.Unicafe(unicafe) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Data.Char (toLower)
import Data.Aeson
import Data.Aeson.TH
import Data.DateTime
import qualified Data.Text as T hiding (map,concat,filter)
import Network.HTTP
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.IO as TIO

import PluginData
import Data.Maybe (catMaybes)

data Restaurant = Metsatalo | Olivia | Porthania | Paarakennus | Rotunda | SocKom | Topelias | Valtiotiede |
                  Kookos | Chemicum | Exactum | Physicum | Meilahti | Ruskeasuo | Biokeskus | Korona | Viikuna | Ylioppilasaukio
                  deriving (Show,Bounded,Eq,Read)

restaurantStrings :: [String]
restaurantStrings = ["Metsätalo", "Olivia", "Porthania", "Päärakennus", "Rotunda", "Sockom",
            "Topelias", "Valtiotiede", "Ylioppilasaukio", "Kookos", "Chemicum", "Exactum",
            "Physicum", "Meilahti", "Ruskeasuo", "Biokeskus", "Korona", "Viikuna"]

data UnicafeData = UnicafeData
  { main_status :: T.Text
  , main_data :: [UnicafeDay]
  } deriving (Show, Eq)

data UnicafeFood = UnicafeFood
  { food_name :: T.Text
  , food_price :: UnicafePrice
  } deriving (Show, Eq)

data UnicafePrice = UnicafePrice
  { price_name :: T.Text
  } deriving (Show, Eq)

data UnicafeDay = UnicafeDay
  { day_data :: [UnicafeFood]
  , day_date :: T.Text
  } deriving (Show, Eq)

days :: UnicafeData -> [UnicafeDay]
days = main_data

foods :: UnicafeDay -> [UnicafeFood]
foods = day_data

foodName :: UnicafeFood -> T.Text
foodName = food_name

foodPrice :: UnicafeFood -> T.Text
foodPrice = price_name . food_price

foodFilter :: UnicafeFood -> Bool
foodFilter f = foodPrice f == "Edullisesti"

dateFilter :: T.Text -> UnicafeDay -> Bool
dateFilter date day = (T.drop 3 $ day_date day) == date

foodNames :: T.Text -> UnicafeData -> [T.Text]
foodNames date = concat . map ((map foodName . filter foodFilter)) . map foods . filter (dateFilter date) . days

$(deriveJSON defaultOptions{fieldLabelModifier = Prelude.drop 5} ''UnicafeData)
$(deriveJSON defaultOptions{fieldLabelModifier = Prelude.drop 4} ''UnicafeDay)
$(deriveJSON defaultOptions{fieldLabelModifier = Prelude.drop 5} ''UnicafeFood)
$(deriveJSON defaultOptions{fieldLabelModifier = Prelude.drop 6} ''UnicafePrice)

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
strToRestaurant s = go $ Prelude.map toLower s
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

unicafeUrl :: Restaurant -> String
unicafeUrl r = "http://hyy-lounastyokalu-production.herokuapp.com/publicapi/restaurant/" ++ (show . fromEnum $ r)

source :: String -> IO B.ByteString
source addr = simpleHTTP (getRequest addr) >>= (fmap C.pack) . getResponseBody

printFood :: (Restaurant, [T.Text]) -> [String]
printFood (r, foods)
  | Prelude.length foods == 0 = [separator '-', show r ++ ":", "No food available today."]
  | otherwise = [separator '-', show r ++ ":"] ++ map T.unpack foods

separator :: Char -> String
separator c = Prelude.take 19 $ repeat c

specialSeparator :: Char -> Char -> String
specialSeparator c s = concat [take 9 normal, [s], take 9 normal]
  where normal = repeat c

currentDate :: IO T.Text
currentDate = toUnicafeDate <$> toGregorian' <$> getCurrentTime

toUnicafeDate :: (Integer, Int, Int) -> T.Text
toUnicafeDate (y,m,d) = T.pack $ intercalate "." [show d, show m]

getFoods :: [Restaurant] -> IO [(Restaurant, [T.Text])]
getFoods = mapM getFood
 where getFood r = do
       src <- source . unicafeUrl $ r
       date <- currentDate
       return $ case eitherDecode src of
         Right (s :: UnicafeData) -> (r, foodNames date s)
         Left err -> (r, [T.pack err])

getRestaurants :: PluginData a -> [Restaurant]
getRestaurants pd =
  case length $ arguments pd of
    0 -> [Chemicum, Exactum]
    _ ->
      case catMaybes . map (strToRestaurant) $ arguments pd of
        [] -> []
        rs -> rs

usage :: [String]
usage = ["Available restaurants: ",
         intercalate ", " restaurantStrings,
         "Example usage: !unicafe Porthania Päärakennus"]

unicafe :: PluginData a -> IO (PluginResult a)
unicafe pd = do
  (d,m,y) <- toGregorian' <$> getCurrentTime
  case getRestaurants pd of
    []  -> msgsToChannel pd $ usage
    restaurants -> do
      foods <- getFoods restaurants
      let foodstrs = ["Food for " ++ intercalate "." [show d,show m,show y] ++ ":"] ++ (concat . map printFood $ foods) ++ [specialSeparator '-' '*']
      msgsToChannel pd foodstrs
