module Plugin(initPlugins, usePlugin, usePluginIO, Plugin(..), reloadPlugins, PluginToLoad(..), HBotPlugin) where

import Config
import PluginData
import Parser (MsgHost)
import System.Plugins.Hotswap as HS
import Control.Monad.Reader
import Control.Exception (try, SomeException)
import Data.Maybe (catMaybes, fromJust)

data PluginToLoad = PluginToLoad { objname :: String, includes :: [String], name :: String, command :: String } | PluginError String
instance Show PluginToLoad where
  show (PluginToLoad obj _ name cmd) = "Name: " ++ name ++ ", Command: " ++ cmd ++ ", Object: " ++ obj

configPath :: String
configPath = "HBot.conf"

getPluginData :: String -> ConfigM PluginToLoad
getPluginData p = do
  s <- getSection p
  case s of
    Just sect -> do
      -- TODO: Handle fromJusts
      Just (_,funcname) <- getItem "Function" (Just $ sectionName sect)
      Just (_,obj) <- getItem "Object" (Just $ sectionName sect)
      return $ PluginToLoad (fromJust obj) [] (fromJust funcname) (sectionName sect)
    Nothing -> return $ PluginError p

pluginsFromConfig :: ConfigM [PluginToLoad]
pluginsFromConfig = mapM getPluginData =<< getSectionKeys "Plugins"

initPlugins :: IO [(String, Plugin a)]
initPlugins = do
  withLoadedConfig configPath $ do
    plugins <- pluginsFromConfig
    liftIO $ mapM createPlugin plugins >>= return . catMaybes

loadOrReload :: PluginToLoad -> [(String, Plugin a)] -> IO (Maybe (String, Plugin a))
loadOrReload plugin oldplugins =
  case lookup (command plugin) oldplugins of
    Just p -> do
      HS.reloadPlugin p
      putStrLn $ "Plugin " ++ (name plugin) ++ " reloaded."
      return $ Just (command plugin, p)
    Nothing -> createPlugin plugin

handlePluginError :: PluginToLoad -> IO()
handlePluginError (PluginToLoad _ _ _ _) = return ()
handlePluginError (PluginError s) = putStrLn s

reloadPlugins :: [(String, Plugin a)] -> IO [(String, Plugin a)]
reloadPlugins oldplugins =
  withLoadedConfig configPath $ do
    plugins <- pluginsFromConfig
    liftIO $ mapM_ handlePluginError plugins
    reloadedplugindata <- liftIO $ mapM (\p -> loadOrReload p oldplugins) plugins
    return . catMaybes $ reloadedplugindata

createPlugin :: PluginToLoad -> IO (Maybe (String, HS.Plugin a))
createPlugin p = do
  putStr $ "Plugin: " ++ (show p)
  pluginload <- try $ HS.newPlugin (objname p) (includes p) (name p) :: IO (Either SomeException (Plugin a))
  case pluginload of
    Left _ -> do
      putStrLn $ " - FAILED to load!"
      return Nothing
    Right okplugin -> do
      putStrLn " - OK!"
      return $ Just (command p, okplugin)
