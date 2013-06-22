module Plugin(initPlugins, usePlugin, usePluginIO, Plugin(..)) where
import System.Plugins.Hotswap

data PluginToLoad = PluginToLoad { objname :: String, includes :: [String], name :: String, command :: String }

plugins = [PluginToLoad "Plugin/Wikla.o" [] "wiklaPlugin" "!wikla",
           PluginToLoad "Plugin/Admin.o" [] "adminCommand" "!admin"]

initPlugins = mapM createPlugin plugins

createPlugin :: PluginToLoad -> IO(String, Plugin a)
createPlugin p = do
  plugin <- newPlugin (objname p) (includes p) (name p)
  return $ (command p, plugin)
