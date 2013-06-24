module Plugin(initPlugins, usePlugin, usePluginIO, Plugin(..), reloadPlugins) where

import Config
import System.Plugins.Hotswap
import Data.Maybe

data PluginToLoad = PluginToLoad { objname :: String, includes :: [String], name :: String, command :: String }
instance Show PluginToLoad where
  show (PluginToLoad obj inc name cmd) = "Name: " ++ name ++ ", Command: " ++ cmd ++ ", Object: " ++ obj

configPath = "HBot.conf"

convertMaybe (a,b) =
  case b of
    Just x  -> (a,x)
    Nothing -> (a,"")

getPluginData c p = do
  s <- getSection c p
  let s' = map convertMaybe s
  let func = fromMaybe "" (lookup "Function" s')
  let obj  = fromMaybe "" (lookup "Object"  s')
  return $ PluginToLoad obj [] func p

pluginsFromConfig c = do
  plugins <- getSection c "Plugins"
  mapM (\p -> getPluginData c p) (map fst plugins)

initPlugins = do
  withLoadedConfig configPath $ \c -> do
    plugins <- pluginsFromConfig c
    mapM createPlugin plugins

loadOrReload plugin oldplugins = do
  case lookup (name plugin) oldplugins of
    Just p  -> reloadPlugin p
    Nothing -> createPlugin plugin >> return ()

reloadPlugins oldplugins = do
  withLoadedConfig configPath $ \c -> do
    plugins <- pluginsFromConfig c
    mapM_ (\p -> loadOrReload p oldplugins) plugins

createPlugin :: PluginToLoad -> IO(String, Plugin a)
createPlugin p = do
  putStrLn $ "Plugin: " ++ (show p)
  plugin <- newPlugin (objname p) (includes p) (name p)
  return $ (command p, plugin)
