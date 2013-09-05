{-# LANGUAGE OverloadedStrings #-}

module UrlAnalyzer(getTitle) where

import Network.HTTP
import Network.Browser
import qualified Data.ByteString.Char8 as B
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast.Utf8Only
import qualified Data.Text as T

source :: String -> IO String
source uri = do
  (_, rsp) <- browse $ do
    setAllowRedirects True
    setMaxRedirects (Just 5)
    setOutHandler $ const (return ())
    request $ getRequest uri
  return $ rspBody rsp

getTitle :: String -> IO String
getTitle uri = do
  src <- source uri
  return $ "[" ++ uri ++ "]: " ++
      (take 500 .
      head .
      map T.unpack .
      map fromTagText .
      filter isTagText .
      takeWhile (not . isTagCloseName "title") .
      dropWhile (not . isTagOpenName "title") .
      parseTagsT .
      B.pack $ src)
