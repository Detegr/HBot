module Plugin.Admin(adminCommand) where

adminCommand params = putStrLn . head $ params
