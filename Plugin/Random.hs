module Plugin.Random(randomQuote) where

import Parser
import Connection
import PluginData
import Plugin.Util.Random
import Control.Monad (replicateM)

invalid pd = msgToChannel pd $ "Allowed number of randoms: 1-5"

randomQuote :: PluginData -> IO PluginResult
randomQuote pd =
  case arguments pd of
    []   -> msgToChannel pd =<< getRandom
    args ->
      case reads . head $ args of
        [(i, "")] -> if i>0 && i<=5
                       then replicateM i getRandom >>= msgsToChannel pd
                       else invalid pd
        _         -> invalid pd
