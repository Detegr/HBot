module Plugin.Random3(random3) where

import Plugin.Random
import PluginData

random3 :: PluginData a -> IO (PluginResult a)
random3 (a,b,_,st) = randomQuote (a,b,["3"],st)
