module Plugin.Error(pluginError) where
  import PluginData

  pluginError :: PluginData a -> IO (PluginResult a)
  pluginError pd = msgToChannel pd "This plugin didn't compile correctly"
