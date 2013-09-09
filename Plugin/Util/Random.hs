module Plugin.Util.Random(getRandom) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.List (intercalate)
import Text.Printf
import System.Locale (defaultTimeLocale)
import Control.Monad ((<=<))
import Plugin.Util.DatabasePath

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
  msg <- fmap head $ quickQuery conn "select * from messages where rowid=(abs(random()) % (select max(rowid)+1 from messages))" []
  if (msg == []) || (length (text msg) < 5)
    then randomMsg conn
    else return msg

getRandom :: IO String
getRandom = do
  conn <- connectSqlite3 dbPath
  msg <- randomMsg conn
  user <- quickQuery' conn "SELECT * from users where id=?" [msg !! userid]
  timestr <- timeToStr $ time msg
  return $ timestr ++ " <" ++ (nick . head $ user) ++ "> " ++ (text msg)
