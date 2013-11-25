module Plugin.Help(help) where

import Control.Monad.State (liftIO)
import PluginData
import Config

help :: PluginData a -> IO (PluginResult a)
help pd = withLoadedConfig "HBot.conf" (getSectionKeys "Plugins") >>= msgsToNick pd
