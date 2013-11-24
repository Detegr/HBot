module Plugin.Logger(logMessage) where

import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Data.Time.Clock
import PluginData
import Parser
import Data.List

channels :: [String]
channels=["#tapiiri"]

logMessage :: PluginData -> IO PluginResult 
logMessage pd = do
  c <- connectPostgreSQL "host=localhost dbname=loggerdb user=postgres"
  putStrLn $ show (head $ head (PluginData.params pd))
  if elem (head . PluginData.params $ pd) channels
  then logMessage' c (nickName . host $ pd) (intercalate " " . arguments $ pd)
  else return ()
  disconnect c
  return NoResult

logMessage' :: IConnection c => c -> String -> String -> IO()
logMessage' c name message = do
    currenttime <- getCurrentTime
    logMessageWithTime c name message currenttime

createNewUser :: IConnection c => c -> String -> IO Int
createNewUser c name = do
    insert <- prepare c "INSERT INTO users(name) VALUES(?) returning id;"
    execute insert [toSql name]
 
    commit c
 
    idresult <- fetchRow insert
 
    let userid = case idresult of
                    Just result -> fromSql $ head result
                    Nothing -> 12345
 
    insertamount <- prepare c "INSERT INTO usermessageamounts(userid, nummessages) VALUES(?, 0);"
    execute insertamount [toSql userid]
 
    commit c
 
    return userid
 
selectOrCreateUser :: IConnection c => c -> String -> IO Int
selectOrCreateUser c name = do
    userselect <- prepare c "SELECT id from users where name=?;"
    execute userselect [toSql name]
 
    userresult <- fetchRow userselect
    userid <- case userresult of
                    Just result -> return . fromSql $ head result
                    Nothing -> createNewUser c name
 
    return userid
 
logMessageWithTime :: IConnection c => c -> String -> String -> UTCTime -> IO()
logMessageWithTime c name message time = do
    userid <- selectOrCreateUser c name
 
    messageinsert <- prepare c "INSERT INTO messages(userid, message, tags, time) VALUES(?,?,to_tsvector('finnish', ?),?) returning id;"
    updateamount <- prepare c "UPDATE usermessageamounts SET nummessages = nummessages+1 WHERE userid=? returning nummessages;"
 
    execute messageinsert [toSql userid, toSql message, toSql message, toSql time]
    execute updateamount [toSql userid]
 
    commit c
 
    Just messageamount <- fetchRow updateamount
    Just messageid <- fetchRow messageinsert
 
    insertusermessage <- prepare c "INSERT INTO usermessages(userid, messagenum, messageid) VALUES(?,?,?);"
 
    execute insertusermessage [toSql userid, head messageamount, head messageid]
 
    commit c
