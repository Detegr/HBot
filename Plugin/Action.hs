module Plugin.Action where

doAction (x:y:xs) =
  case x of
    "join" -> 
