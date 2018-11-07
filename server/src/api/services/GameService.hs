{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Services.GameService where

import Control.Monad.IO.Class
import Database.MongoDB
import Database.MongoDB.Query as MQ
import Database.MongoDB.Connection
import Data.Bson as BS
import Api.Types
import Control.Lens
import Control.Monad.State.Class
import Data.Aeson
import Data.UUID as UUID
import Data.UUID.V4
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B
import Data.Text as T
import Data.Time.Clock.POSIX

data GameService = GameService { }

makeLenses ''GameService

gameRoutes :: Host -> Username -> Password -> Database -> [(B.ByteString, Handler b GameService ())]
gameRoutes mongoHost mongoUser mongoPass mongoDb = [
    ("/", method GET $ getPublicGamesList mongoHost mongoUser mongoPass mongoDb),
    ("/", method POST $ createGame mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/setmap", method POST $ sendMap mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session", method GET $ getStatus mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/invitebot", method POST $ inviteBot mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/setpublic", method POST $ setPublic mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/connect/:role", method POST $ connectGame mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/shoot", method POST $ shoot mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/chat", method POST $ sendMessage mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/chat", method GET $ readMessages mongoHost mongoUser mongoPass mongoDb)
  ]

getPublicGamesList :: Host -> Username -> Password -> Database -> Handler b GameService ()
getPublicGamesList mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse $ setHeader "Content-Type" "application/json"
  time <- liftIO $ fmap round getPOSIXTime
  games <- liftIO $ a $ (rest =<< MQ.find (MQ.select ["date" =: ["$gte" =: time - 3600]] "games"))
  writeLBS . encode $ fmap (\d -> PublicGame (BS.at "game" d) (BS.at "name" (BS.at "owner" d)) (BS.at "message" d)) games
  modifyResponse . setResponseCode $ 200

createGame :: Host -> Username -> Password -> Database -> Handler b GameService ()
createGame mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse $ setHeader "Content-Type" "application/json"
  user <- fmap decode $ readRequestBody 4096
  case user of Just (NewGameUser name message) -> do
                 gameId <- liftIO $ UUID.toString <$> nextRandom
                 sessionId <- liftIO $ UUID.toString <$> nextRandom
                 time <- liftIO $ fmap round getPOSIXTime
                 let game = [
                              "game" =: gameId,
                              "date" =: time,
                              "message" =: "",
                              "owner" =: ["name" =: name, "message" =: message, "session" =: sessionId],
                              "turn" =: "notready",
                              "public" =: False,
                              "guest" =: ([]::[Field]),
                              "chat" =: ([]::[Field])
                            ]
                 liftIO $ a $ MQ.insert "games" game
                 writeLBS $ encode $ NewGame gameId sessionId
                 modifyResponse $ setResponseCode 201
               Nothing -> do
                 writeLBS . encode $ APIError "Name and message can't be empty!"
                 modifyResponse $ setResponseCode 400

sendMap :: Host -> Username -> Password -> Database -> Handler b GameService ()
sendMap mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse . setResponseCode $ 202

getStatus :: Host -> Username -> Password -> Database -> Handler b GameService ()
getStatus mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse . setResponseCode $ 200

inviteBot :: Host -> Username -> Password -> Database -> Handler b GameService ()
inviteBot mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse . setResponseCode $ 501

setPublic :: Host -> Username -> Password -> Database -> Handler b GameService ()
setPublic mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse . setResponseCode $ 200

connectGame :: Host -> Username -> Password -> Database -> Handler b GameService ()
connectGame mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse . setResponseCode $ 202

shoot :: Host -> Username -> Password -> Database -> Handler b GameService ()
shoot mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse . setResponseCode $ 202

sendMessage :: Host -> Username -> Password -> Database -> Handler b GameService ()
sendMessage mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse . setResponseCode $ 201

readMessages :: Host -> Username -> Password -> Database -> Handler b GameService ()
readMessages mongoHost mongoUser mongoPass mongoDb = do
  let a = performAction mongoHost mongoUser mongoPass mongoDb
  modifyResponse . setResponseCode $ 200

performAction :: Host -> Username -> Password -> Database -> Action IO a -> IO a
performAction mongoHost mongoUser mongoPass mongoDb action = do
  pipe <- connect mongoHost
  access pipe master mongoDb $ auth mongoUser mongoPass
  r <- access pipe master mongoDb $ action
  close pipe
  return r

gameServiceInit :: String -> String -> String -> String -> SnapletInit b GameService
gameServiceInit mongoHost mongoUser mongoPass mongoDb = makeSnaplet "game" "Battleship Service" Nothing $ do
  addRoutes $ gameRoutes (readHostPort mongoHost) (T.pack mongoUser) (T.pack mongoPass) (T.pack mongoDb)
  return $ GameService
