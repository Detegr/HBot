module Plugin.Util.Random(getRandom) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.DateTime
import Data.List (intercalate)
import Text.Printf

nick xs = fromSql (xs !! 1) :: String
text = nick
userid = 2
time xs = fromSql (xs !! 3) :: DateTime

timeToStr :: DateTime -> String
timeToStr dt = let (_,_,_,h,m,_) = toGregorian dt
               in intercalate ":" [printf "%02d" h, printf "%02d" m]

randomMsg :: Connection -> IO [SqlValue]
randomMsg conn = do
  msg <- fmap head $ quickQuery conn "select * from messages where rowid=(abs(random()) % (select max(rowid)+1 from messages))" []
  if (msg == []) || (length (text msg) < 5)
    then randomMsg conn
    else return msg

getRandom :: IO String
getRandom = do
  conn <- connectSqlite3 "production.sqlite3"
  msg <- randomMsg conn
  user <- quickQuery' conn "SELECT * from users where id=?" [msg !! userid]
  return $ (timeToStr . time $ msg) ++ " <" ++ (nick . head $ user) ++ "> " ++ (text msg)
