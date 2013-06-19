module Plugin(initPlugins, runPlugin, Plugin(..)) where
import Plugin.Wikla
import System.Plugins.Hotswap

data PluginToLoad = PluginToLoad { objname :: String, includes :: [String], name :: String, command :: String }

plugins = [PluginToLoad "Plugin/Wikla.o" [] "wiklaPlugin" "!wikla",
           PluginToLoad "Plugin/Admin.o" [] "adminCommand" "!admin"]

initPlugins = mapM createPlugin plugins

createPlugin :: PluginToLoad -> IO(String, Plugin a)
createPlugin p = do
  plugin <- newPlugin (objname p) (includes p) (name p)
  return $ (command p, plugin)

main = initPlugins >>= \p -> case lookup "!admin" p of
                                  Just plg -> usePluginIO plg ["bar"]
                                  _ -> putStrLn "not found"
