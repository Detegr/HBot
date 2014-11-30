module Plugin.Trivia(trivia) where

import PluginData
import Plugin.Util.Random
import Data.Char (toLower)
import qualified Data.HashMap as H
import Data.String.Utils (strip)
    
parseUserAndMessage :: String -> (String, String)
parseUserAndMessage s = (parseUser s, parseMessage s)

parseMessage :: String -> String
parseMessage = drop 2 . dropWhile (/= '>')

parseUser :: String -> String
parseUser = takeWhile (/= '>') . tail . dropWhile (/= '<')

question :: String
question = "Guess the author of this line: "

correct :: String -> String -> H.Map String Int -> String
correct w u m = "The winner is " ++ w ++ "(" ++ (show (m H.! w)) ++ "), that line was written by " ++ u ++ "."

incorrect :: String
incorrect = "Nope."

cancel :: String -> String
cancel u = "Cancelling trivia, the correct answer was: " ++ u

addPoints :: H.Map String Int -> String -> Int -> H.Map String Int
addPoints m u p =
  if H.member u m
    then H.insert u ((m H.! u)+p) m
    else H.insert u p m

type TriviaData = (Maybe (String, String), H.Map String Int)

trivia :: PluginData TriviaData -> IO (PluginResult TriviaData)
trivia pd = do
  if (head $ head (params pd)) /= '#'
    then msgTo (getNick pd) "Dude, leave me alone!" (state pd)
    else do
      let a = arguments pd
      case (state pd) of
        Just (mbu,points) ->
          case mbu of
            Just (u,msg) ->
              if length a > 0 
                then if (map toLower (strip . head $ a) == map toLower u)
                  then do
                    let newpoints=addPoints points (getNick pd) 1
                    msgToChannel' pd (correct (getNick pd) u newpoints) $ Just (Nothing, newpoints)
                  else msgToChannel' pd incorrect $ Just (Just (u,msg), addPoints points (getNick pd) (-1))
                else msgToChannel' pd (cancel u) $ Just (Nothing, points)
            Nothing ->
              if length a > 0
                then msgToChannel' pd "Launch trivia without arguments." $ Just (Nothing, points)
                else do
                  (u,msg) <- fmap parseUserAndMessage $ getRandom Nothing
                  msgToChannel' pd (question ++ msg) $ Just (Just (u,msg), points)
        Nothing -> trivia ((host pd), (params pd), (arguments pd), Just (Nothing, H.empty))
