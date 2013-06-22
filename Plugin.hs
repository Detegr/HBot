module Plugin(initPlugins, usePlugin, usePluginIO, Plugin(..)) where

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

initPlugins = do
  withLoadedConfig configPath $ \c -> do
    plugins <- getSection c "Plugins"
    plugins' <- mapM (\p -> getPluginData c p) (map fst plugins)
    mapM createPlugin plugins'

createPlugin :: PluginToLoad -> IO(String, Plugin a)
createPlugin p = do
  putStrLn $ "Plugin: " ++ (show p)
  plugin <- newPlugin (objname p) (includes p) (name p)
  return $ (command p, plugin)
