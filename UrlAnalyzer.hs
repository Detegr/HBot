{-# LANGUAGE OverloadedStrings #-}

module UrlAnalyzer(getTitle) where

import Data.Text.Lazy.Encoding
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast.Utf8Only

getTitle :: String -> IO String
getTitle uri = do
  src <- simpleHttp uri
  let mbtitle=(map B.unpack .
               map fromTagText .
               filter isTagText .
               takeWhile (not . isTagCloseName "title") .
               dropWhile (not . isTagOpenName "title") .
               parseTags . B.concat . L.toChunks $ src)
  if mbtitle == []
    then return "No title"
    else return $ "Title: " ++ (take 500 . head $ mbtitle)
