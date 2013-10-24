module Plugin.Util.Random(getRandom) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.List (intercalate)
import Text.Printf
import System.Locale (defaultTimeLocale)
import Plugin.Util.DatabasePath
import Control.Exception (try)

nick xs = fromSql (xs !! 1) :: String
text = nick
userid = 2
time xs = fromSql (xs !! 3) :: UTCTime

timeToStr :: UTCTime -> IO String
timeToStr dt = do
  tz <- getCurrentTimeZone
  return $ formatTime defaultTimeLocale "%H:%M" (utcToLocalTime tz dt)

randomMsg :: Connection -> IO [SqlValue]
randomMsg conn = do
  em <- try . handleSqlError $ quickQuery conn "select * from messages where rowid=(abs(random()) % (select max(rowid)+1 from messages))" [] :: IO (Either IOError [[SqlValue]])
  case em of
    Left _ -> randomMsg conn
    Right m -> do
      let msg=head m
      if (msg == []) || (length (text msg) < 5)
        then randomMsg conn
        else return msg

getRandom :: IO String
getRandom = do
  conn <- connectSqlite3 dbPath
  msg <- randomMsg conn
  eu <- try . handleSqlError $ quickQuery' conn "SELECT * from users where id=?" [msg !! userid] :: IO (Either IOError [[SqlValue]])
  case eu of
    Left _ -> getRandom
    Right user -> do
      timestr <- timeToStr $ time msg
      return $ timestr ++ " <" ++ (nick . head $ user) ++ "> " ++ (text msg)
