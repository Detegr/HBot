{-# LANGUAGE OverloadedStrings #-}

--module Plugin.Random(randomQuote) where

import Text.HTML.TagSoup hiding (renderTags)
import Text.HTML.TagSoup.Fast.Utf8Only
import qualified Data.Text as T
import Data.ByteString.Char8 hiding (dropWhile,takeWhile,head,words,putStrLn)
import Network.HTTP
import Text.Regex.TDFA.Text
import Text.Regex.Base.RegexLike
import Text.Parsec
import Text.Parsec.Text
import Control.Monad
import Data.Dates
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Text.IO (putStrLn)

--import Parser
--import Connection

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
  t <- try $ string "Maukkaasti" <|> string "Edullisesti"
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

foodsFromSource :: String -> [T.Text]
foodsFromSource = joinFoodAndType .
                  Prelude.filter ((> 1) . T.length) .
                  Prelude.map fromTagText .
                  Prelude.filter isTagText .
                  dropWhile (not . isTagOpenName "li") .
                  parseTagsT . pack

--unicafe  :: (MsgHost, [String], [String]) -> IO (Command String)
unicafe (_,p,a) = do
  (year,week,weekday) <- getCurrentDateTime >>= return . toWeekDate . dateTimeToDay
  putStrLn $ (show week) ++ " " ++ (show weekday)
  mapM_ (Data.Text.IO.putStrLn) =<< fmap foodsFromSource (source $ unicafeurl week weekday year Chemicum)
 where args = words a

{-
   regexec (makeRegex ".*" :: Regex) "foobar"
-}
main = unicafe (undefined,["a"],"")
