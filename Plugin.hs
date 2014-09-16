module Plugin(initPlugins, reloadPlugins, PluginToLoad(..), HBotPlugin) where

import Config
import Control.Exception (try, SomeException)
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromJust)
import DynFlags
import Exception
import GHC hiding (ModuleName)
import GHC.Paths
import Parser (MsgHost)
import PluginData
import Unsafe.Coerce
import System.FilePath.Posix (splitExtension)

type HBotPlugin a = (MsgHost, [String], [String], Maybe a) -> IO (PluginResult a)
data PluginToLoad = PluginToLoad { cmd :: String, source :: String, mod :: String, function :: String } | PluginError String
instance Show PluginToLoad where
  show (PluginToLoad command src mod func) = "Command:" ++ command ++ "Source: " ++ src ++ ", Module: " ++ mod ++ ", Function: " ++ func

configPath :: String
configPath = "HBot.conf"

type ModuleName = String
type FuncName = String

loadPlugin :: ModuleName -> FuncName -> Ghc HValue
loadPlugin mod func = do
    liftIO $ putStrLn $ "Loading " ++ mod ++ " " ++ func
    setContext [IIDecl $ simpleImportDecl (mkModuleName mod)]
    compileExpr (intercalate "." [mod, func])

compilePlugin :: PluginToLoad -> IO (FuncName, (Maybe a, HBotPlugin a))
compilePlugin plugin =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      compiledFunc <- runGhc (Just libdir) $ do
        setSessionDynFlags =<< getSessionDynFlags
        mapM (flip guessTarget Nothing) [source plugin, "Plugin/Error.hs"] >>= setTargets
        r <- load LoadAllTargets
        case r of
          Failed    -> loadPlugin "Plugin.Error" "pluginError"
          Succeeded -> loadPlugin (Plugin.mod plugin) (function plugin)
      return (Plugin.cmd plugin, (Nothing, unsafeCoerce compiledFunc :: HBotPlugin a))

getPluginData :: String -> ConfigM PluginToLoad
getPluginData p = do
  s <- getSection p
  case s of
    Just sect -> do
      -- TODO: Handle fromJusts
      Just (_,funcname) <- getItem "Function" (Just $ sectionName sect)
      Just (_,src) <- getItem "Source" (Just $ sectionName sect)
      let (base,ext) = splitExtension $ fromJust src
      let mod = map (\c -> if c == '/' then '.' else c) base
      return $ PluginToLoad (sectionName sect) (fromJust src) mod (fromJust funcname)
    Nothing -> return $ PluginError p

pluginsFromConfig :: ConfigM [PluginToLoad]
pluginsFromConfig = mapM getPluginData =<< getSectionKeys "Plugins"

initPlugins :: IO [(FuncName, (Maybe a, HBotPlugin a))]
initPlugins = do
  withLoadedConfig configPath $ do
    plugins <- pluginsFromConfig
    liftIO $ mapM compilePlugin plugins

handlePluginError :: PluginToLoad -> IO()
handlePluginError (PluginToLoad _ _ _ _) = return ()
handlePluginError (PluginError s) = putStrLn s

reloadPlugins :: [(FuncName, (Maybe a, HBotPlugin a))] -> IO [(FuncName, (Maybe a, HBotPlugin a))]
reloadPlugins _ = initPlugins
