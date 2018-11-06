{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.GameService where

import Control.Monad.IO.Class
import Database.MongoDB
import Database.MongoDB.Query as MQ
import Database.MongoDB.Connection
import Api.Types
import Control.Lens
import Control.Monad.State.Class
import Data.Aeson
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B
import Data.Text as T

data GameService = GameService { }

makeLenses ''GameService

gameRoutes :: Host -> Username -> Password -> Database -> [(B.ByteString, Handler b GameService ())]
gameRoutes mongoHost mongoUser mongoPass mongoDatabase = [
    ("/", method GET $ getPublicGamesList mongoHost mongoUser mongoPass mongoDatabase),
    ("/", method POST $ createGame mongoHost mongoUser mongoPass mongoDatabase),
    ("/:gameid/:session/setmap", method POST $ sendMap mongoHost mongoUser mongoPass mongoDatabase),
    ("/:gameid/:session", method GET $ getStatus mongoHost mongoUser mongoPass mongoDatabase),
    ("/:gameid/:session/invitebot", method POST $ inviteBot mongoHost mongoUser mongoPass mongoDatabase),
    ("/:gameid/:session/setpublic", method POST $ setPublic mongoHost mongoUser mongoPass mongoDatabase),
    ("/:gameid/:session/connect/:role", method POST $ connectGame mongoHost mongoUser mongoPass mongoDatabase),
    ("/:gameid/:session/shoot", method POST $ shoot mongoHost mongoUser mongoPass mongoDatabase),
    ("/:gameid/:session/chat", method POST $ sendMessage mongoHost mongoUser mongoPass mongoDatabase),
    ("/:gameid/:session/chat", method GET $ readMessages mongoHost mongoUser mongoPass mongoDatabase)
  ]

getPublicGamesList :: Host -> Username -> Password -> Database -> Handler b GameService ()
getPublicGamesList mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  games <- liftIO $ performAction mongoHost mongoUser mongoPass mongoDatabase $ (rest =<< MQ.find (MQ.select [] "games"))
  mapM_ (mapM_ (writeLBS . encode)) games
  writeLBS . encode $ [Game 0 "test 0", Game 1 "test 1"]
  modifyResponse . setResponseCode $ 200

createGame :: Host -> Username -> Password -> Database -> Handler b GameService ()
createGame mongoHost mongoUser mongoPass mongoDatabase = do
  ownerName <- getParam "name"
  message <- getParam "message"
  body <- readRequestBody 4096
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ [ownerName, message]
  writeLBS . encode $ body
  modifyResponse $ setResponseCode 201

sendMap :: Host -> Username -> Password -> Database -> Handler b GameService ()
sendMap mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse . setResponseCode $ 202

getStatus :: Host -> Username -> Password -> Database -> Handler b GameService ()
getStatus mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse . setResponseCode $ 200

inviteBot :: Host -> Username -> Password -> Database -> Handler b GameService ()
inviteBot mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse . setResponseCode $ 501

setPublic :: Host -> Username -> Password -> Database -> Handler b GameService ()
setPublic mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse . setResponseCode $ 200

connectGame :: Host -> Username -> Password -> Database -> Handler b GameService ()
connectGame mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse . setResponseCode $ 202

shoot :: Host -> Username -> Password -> Database -> Handler b GameService ()
shoot mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse . setResponseCode $ 202

sendMessage :: Host -> Username -> Password -> Database -> Handler b GameService ()
sendMessage mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse . setResponseCode $ 201

readMessages :: Host -> Username -> Password -> Database -> Handler b GameService ()
readMessages mongoHost mongoUser mongoPass mongoDatabase = do
  modifyResponse . setResponseCode $ 200

performAction :: Host -> Username -> Password -> Database -> Action IO a -> IO a
performAction mongoHost mongoUser mongoPass mongoDatabase action = do
  pipe <- connect mongoHost
  au <- access pipe master mongoDatabase $ auth mongoUser mongoPass
  r <- access pipe master mongoDatabase $ action
  close pipe
  return r

gameServiceInit :: String -> String -> String -> String -> SnapletInit b GameService
gameServiceInit mongoHost mongoUser mongoPass mongoDb = makeSnaplet "game" "Battleship Service" Nothing $ do
  addRoutes (gameRoutes (readHostPort mongoHost) (T.pack mongoUser) (T.pack mongoPass) (T.pack mongoDb))
  return $ GameService
