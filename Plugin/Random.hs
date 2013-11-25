module Plugin.Random(randomQuote) where

import Parser
import Connection
import PluginData
import Plugin.Util.Random
import Control.Monad (replicateM)
import Plugin.Util.DatabasePath

invalid pd = msgToChannel pd $ "Allowed number of randoms: 1-5"

randomQuote :: PluginData a -> IO (PluginResult a)
randomQuote pd =
  case arguments pd of
    []   -> msgToChannel pd =<< getRandom Nothing
    args ->
      case reads . head $ args of
        [(i, "")] -> if i>0 && i<=5
                       then do
                         let user = if (length args)>=2 then Just (args !! 1) else Nothing
                         replicateM i (getRandom user) >>= msgsToChannel pd
                       else invalid pd
        _         -> invalid pd
