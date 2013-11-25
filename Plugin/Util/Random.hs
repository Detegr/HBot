module Plugin.Util.Random(getRandom) where

import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.List (intercalate)
import Text.Printf
import System.Locale (defaultTimeLocale)
import Plugin.Util.DatabasePath
import Control.Exception (try)

nick xs = fromSql (xs !! 1) :: String
text xs = fromSql (xs !! 2) :: String
userid = 1
time xs = localTimeToUTC utc (fromSql (xs !! 4) :: LocalTime)

timeToStr :: UTCTime -> IO String
timeToStr dt = do
  tz <- getCurrentTimeZone
  return $ formatTime defaultTimeLocale "%H:%M" (utcToLocalTime tz dt)

randomMsg :: IConnection c => c -> IO [SqlValue]
randomMsg conn = do
  em <- try . handleSqlError $ quickQuery conn "select * from messages offset floor(random()*(select SUM(nummessages) from usermessageamounts)) limit 1;" [] :: IO (Either IOError [[SqlValue]])
  case em of
    Left _ -> randomMsg conn
    Right m -> do
      let msg=head m
      if (msg == []) || (length (text msg) < 5)
        then randomMsg conn
        else return msg

randomMsgForUser :: IConnection c => c -> String -> IO String
randomMsgForUser c u = do
  eu <- try . handleSqlError $ quickQuery' c "SELECT * from users where name ILIKE ?" [toSql u] :: IO (Either IOError [[SqlValue]])
  case eu of
    Left _ -> return $ "No such user: " ++ u
    Right user -> do
      if user == []
        then return $ "No such user: " ++ u
        else do
          let uid=head . head $ user
          em <- try . handleSqlError $
                quickQuery c "select * from messages where id=(select messageid from usermessages where userid=? AND messagenum=(select floor(random()*(select nummessages from usermessageamounts where userid=?))));" [uid, uid] :: IO (Either IOError [[SqlValue]])
          case em of
            Left _ -> return "Something went wrong."
            Right m -> do
              timestr <- timeToStr . time . head $! m
              return $ timestr ++ " <" ++ (nick . head $ user) ++ "> " ++ (text . head $ m)

getRandom :: Maybe String -> IO String
getRandom mbu = do
  conn <- connectPostgreSQL "host=localhost dbname=loggerdb user=postgres"
  case mbu of
    Nothing -> do
      msg <- randomMsg conn
      eu <- try . handleSqlError $ quickQuery' conn "SELECT * from users where id=?" [msg !! userid] :: IO (Either IOError [[SqlValue]])
      case eu of
        Left _ -> disconnect conn >> getRandom mbu
        Right user -> do
          timestr <- timeToStr $ time msg
          disconnect conn
          return $ timestr ++ " <" ++ (nick . head $ user) ++ "> " ++ (text msg)
    Just u -> do
      str <- randomMsgForUser conn u
      disconnect conn
      return str
