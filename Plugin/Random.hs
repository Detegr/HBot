module Plugin.Random(randomQuote) where

import Text.HTML.TagSoup
import Network.HTTP
import Parser
import Connection

source addr = simpleHTTP (getRequest addr) >>= getResponseBody

randomQuote :: (MsgHost, [String], [String]) -> IO (Command String)
randomQuote (_,p,_) = do
  src <- source "http://muum.org/stats/channels/1/random"
  let txt=head . dropWhile (not . isTagText) . takeWhile (not . isTagClose) . dropWhile (not . isTagOpenName "span") $ parseTags src
  case maybeTagText txt of
    Just s -> return $ PRIVMSG (head p) s
    _      -> return $ PRIVMSG (head p) "Error fetching random quote :("
