module Plugin.Help(help) where

import Control.Monad.State (liftIO)
import PluginData
import Config

help :: PluginData -> IO PluginResult
help pd = withLoadedConfig "HBot.conf" (getSectionKeys "Plugins") >>= msgsToNick pd
