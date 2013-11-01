{-# LANGUAGE OverloadedStrings #-}

module Plugin.UrlAnalyzer(analyzeUrl) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast.Utf8Only
import Data.Maybe(catMaybes)
import Control.Exception(try,SomeException)
import Network.URI
import Data.List (isPrefixOf)

import PluginData

getTitle :: String -> IO String
getTitle uri = do
  src <- simpleHttp uri
  let mbtitle=(map T.unpack .
               map fromTagText .
               filter isTagText .
               takeWhile (not . isTagCloseName "title") .
               dropWhile (not . isTagOpenName "title") .
               parseTagsT . B.concat . L.toChunks $ src)
  if mbtitle == []
    then return "No title"
    else return $ "Title: " ++ (take 500 . head $ mbtitle)

analyzeUrl :: PluginData -> IO PluginResult
analyzeUrl pd = do
  putStrLn $ "Analyzer: " ++ (getChannel pd) ++ " " ++ (show . arguments $ pd)
  urls <- analyzeUrls (arguments pd)
  case urls of 
    Just urls -> msgsToChannel pd urls
    Nothing -> return NoResult

analyzeUrls :: [String] -> IO (Maybe [String])
analyzeUrls strs = do
  let urls=catMaybes . map analyze $ strs
  urls' <- (try (mapM getTitle urls) :: IO (Either SomeException [String]))
  return $ case urls' of
    Left _ -> Nothing
    Right urls'' -> Just urls''
 where analyze s =
         case parseURI s of
           Just u  -> if uriScheme u == "http:" || uriScheme u == "https:"
                        then Just s
                        else Nothing
           Nothing -> if isPrefixOf "www." s
                        then Just $ "http://" ++ s
                        else Nothing
