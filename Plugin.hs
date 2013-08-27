module Plugin(initPlugins, usePlugin, usePluginIO, Plugin(..), reloadPlugins, PluginToLoad(..)) where

import Config
import System.Plugins.Hotswap as HS
import Data.Maybe
import Control.Monad.Reader

data PluginToLoad = PluginToLoad { objname :: String, includes :: [String], name :: String, command :: String } | PluginError String
instance Show PluginToLoad where
  show (PluginToLoad obj inc name cmd) = "Name: " ++ name ++ ", Command: " ++ cmd ++ ", Object: " ++ obj

configPath = "HBot.conf"

getPluginData p = do
  s <- getSection p
  case s of
    Just s -> do
      Just (funcname,_) <- getItem "Function" (Just $ sectionName s)
      Just (key,val) <- getItem "Object" (Just $ sectionName s)
      return $ PluginToLoad key [] funcname (sectionName s)
    Nothing -> return $ PluginError p

pluginsFromConfig = mapM getPluginData =<< getSectionKeys "Plugins"

initPlugins = do
  withLoadedConfig configPath $ do
    plugins <- pluginsFromConfig
    return $ mapM createPlugin plugins

loadOrReload :: PluginToLoad -> [(String, Plugin a)] -> IO(String, Plugin a)
loadOrReload plugin oldplugins =
  case lookup (command plugin) oldplugins of
    Just p -> do
      HS.reloadPlugin p
      putStrLn $ "Plugin " ++ (name plugin) ++ " reloaded."
      return (command plugin, p)
    Nothing -> createPlugin plugin

reloadPlugins oldplugins =
  withLoadedConfig configPath $ do
    plugins <- pluginsFromConfig
    reloadedplugindata <- liftIO $ mapM (\p -> loadOrReload p oldplugins) plugins
    return reloadedplugindata

createPlugin :: PluginToLoad -> IO(String, HS.Plugin a)
createPlugin p = do
  putStrLn $ "Plugin: " ++ (show p)
  plugin <- HS.newPlugin (objname p) (includes p) (name p)
  return (command p, plugin)
