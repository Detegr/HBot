module Plugin.Random(randomQuote) where

import Text.HTML.TagSoup
import Network.HTTP
import Parser
import Connection
import PluginData

source addr = simpleHTTP (getRequest addr) >>= getResponseBody

randomQuote :: PluginData -> IO PluginResult
randomQuote pd = do
  src <- source "http://muum.org/stats/channels/1/random"
  let txt=head . dropWhile (not . isTagText) . takeWhile (not . isTagClose) . dropWhile (not . isTagOpenName "span") $ parseTags src
  msgToChannel pd $
    case maybeTagText txt of
      Just s -> s
      _      -> "Error fetching random quote :("
