module Plugin.Random3(random3) where

import Plugin.Random
import PluginData

random3 :: PluginData -> IO PluginResult
random3 (a,b,_) = randomQuote (a,b,["3"])
