module Plugin.Util.Trivia(trivia) where

import PluginData
import Plugin.Util.Random
import Data.Char (toLower)
    
parseUserAndMessage :: String -> (String, String)
parseUserAndMessage s = (parseUser s, parseMessage s)

parseMessage :: String -> String
parseMessage = drop 2 . dropWhile (/= '>')

parseUser :: String -> String
parseUser = takeWhile (/= '>') . tail . dropWhile (/= '<')

question :: String
question = "Guess the author of this line: "

correct :: String -> String -> String
correct w u = "The winner is " ++ w ++ ", that line was written by " ++ u ++ "."

incorrect :: String
incorrect = "Nope."

cancel :: String -> String
cancel u = "Cancelling trivia, the correct answer was: " ++ u

trivia :: PluginData (String, String) -> IO (PluginResult (String, String))
trivia pd = do
  if (head $ head (params pd)) /= '#'
    then msgTo (getNick pd) "Dude, leave me alone!" (state pd)
    else do
      let a = arguments pd
      case (state pd) of
        Just (u,msg) ->
          if length a > 0 
            then if (map toLower (head a) == map toLower u)
              then msgToChannel' pd (correct (getNick pd) u) Nothing
              else msgToChannel' pd incorrect (state pd)
            else msgToChannel' pd (cancel u) Nothing
        Nothing ->
          if length a > 0
            then msgToChannel' pd "Launch trivia without arguments." Nothing
            else do
              (u,msg) <- fmap parseUserAndMessage $ getRandom Nothing
              msgToChannel' pd (question ++ msg) $ Just (u,msg)
