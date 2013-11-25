module Plugin(initPlugins, usePlugin, usePluginIO, Plugin(..), reloadPlugins, PluginToLoad(..), HBotPlugin) where

import Config
import PluginData
import Parser (MsgHost)
import System.Plugins.Hotswap as HS
import Control.Monad.Reader
import Control.Exception (try, SomeException)
import Data.Maybe (catMaybes, fromJust)

type HBotPlugin a = Plugin ((MsgHost, [String], [String], Maybe a) -> IO (PluginResult a))
data PluginToLoad = PluginToLoad { objname :: String, includes :: [String], name :: String, command :: String } | PluginError String
instance Show PluginToLoad where
  show (PluginToLoad obj includes name cmd) = "Name: " ++ name ++ ", Command: " ++ cmd ++ ", Object: " ++ obj ++ ", Includes: " ++ (show includes)

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
      Just (_,incls) <- getItem "Include" (Just $ sectionName sect)
      return $ PluginToLoad (fromJust obj) (words . fromJust $ incls) (fromJust funcname) (sectionName sect)
    Nothing -> return $ PluginError p

pluginsFromConfig :: ConfigM [PluginToLoad]
pluginsFromConfig = mapM getPluginData =<< getSectionKeys "Plugins"

initPlugins :: IO [(String, (Maybe b, Plugin a))]
initPlugins = do
  withLoadedConfig configPath $ do
    plugins <- pluginsFromConfig
    liftIO $ mapM createPlugin plugins >>= return . catMaybes

loadOrReload :: PluginToLoad -> [(String, (Maybe b, Plugin a))] -> IO (Maybe (String, (Maybe b, Plugin a)))
loadOrReload plugin oldplugins =
  case lookup (command plugin) oldplugins of
    Just (st,p) -> do
      HS.reloadPlugin p
      putStrLn $ "Plugin " ++ (name plugin) ++ " reloaded."
      return $ Just (command plugin, (st,p))
    Nothing -> createPlugin plugin

handlePluginError :: PluginToLoad -> IO()
handlePluginError (PluginToLoad _ _ _ _) = return ()
handlePluginError (PluginError s) = putStrLn s

reloadPlugins :: [(String, (Maybe b, Plugin a))] -> IO [(String, (Maybe b, Plugin a))]
reloadPlugins oldplugins =
  withLoadedConfig configPath $ do
    plugins <- pluginsFromConfig
    liftIO $ mapM_ handlePluginError plugins
    reloadedplugindata <- liftIO $ mapM (\p -> loadOrReload p oldplugins) plugins
    return . catMaybes $ reloadedplugindata

createPlugin :: PluginToLoad -> IO (Maybe (String, (Maybe b, HS.Plugin a)))
createPlugin p = do
  putStr $ "Plugin: " ++ (show p)
  pluginload <- try $ HS.newPlugin (objname p) (includes p) (name p) :: IO (Either SomeException (Plugin a))
  case pluginload of
    Left e -> do
      putStrLn $ " - FAILED to load! " ++ (show e)
      return Nothing
    Right okplugin -> do
      putStrLn " - OK!"
      return $ Just (command p, (Nothing, okplugin))
