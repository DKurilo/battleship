{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Play where

import qualified Data.Text as T
import Control.Concurrent (threadDelay)
import Types
import Database.MongoDB
import Database.MongoDB.Query
import Database.MongoDB.Connection
import Network.Connection
import Network.HTTP.Conduit
import Data.Aeson

readGamesList :: String -> String -> IO [String]
readGamesList url name = do
      request <- parseRequest $ url ++ "/bots/" ++ name ++ "/games"
      let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      manager <- newManager settings
      res <- fmap responseBody $ httpLbs request manager
      let mbgames = decode $ res :: Maybe [String]
      case mbgames of (Just games) -> return games
                      _ -> return ([]::[String])

processGame :: String -> (Action IO a -> IO a) -> String -> IO ()
processGame url action gid = do
      putStrLn gid

play :: Int -> String -> String -> String -> String -> String -> String -> IO()
play repeatDelay apiurl botname smongoHost smongoUser smongoPass smongoDb = do
      let mongoHost = readHostPort smongoHost
      let mongoUser = T.pack smongoUser
      let mongoPass = T.pack smongoPass
      let mongoDb = T.pack smongoDb
      play' repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb

play' :: Int -> String -> String -> Host -> Username -> Password -> Database -> IO()
play' repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb = do
      pipe <- connectAndAuth mongoHost mongoUser mongoPass mongoDb
      gamesList <- readGamesList apiurl botname
      mapM_ (processGame apiurl (performAction pipe mongoDb)) gamesList
      closeConnection pipe
      threadDelay (max repeatDelay 1 * 1000000)
      play' repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb

----------------------
-- MongoDB functions
connectAndAuth :: Host -> Username -> Password -> Database -> IO Pipe
connectAndAuth mongoHost mongoUser mongoPass mongoDb = do 
  pipe <- connect mongoHost
  access pipe master mongoDb $ auth mongoUser mongoPass
  return pipe

performAction :: Pipe -> Database -> Action IO a -> IO a
performAction pipe mongoDb action = access pipe master mongoDb action

closeConnection :: Pipe -> IO ()
closeConnection pipe = close pipe
