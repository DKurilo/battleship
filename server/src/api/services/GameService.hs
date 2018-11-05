{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.GameService where

import Types
import Api.Types
import Control.Lens
import Control.Monad.State.Class
import Data.Aeson
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B

data GameService = GameService { }

makeLenses ''GameService

gameRoutes :: [(B.ByteString, Handler b GameService ())]
gameRoutes = [
    ("/", method GET getPublicGamesList),
    ("/", method POST createGame),
    ("/:gameid/:session/setmap", method POST sendMap),
    ("/:gameid/:session", method GET getStatus),
    ("/:gameid/:session/invitebot", method POST inviteBot),
    ("/:gameid/:session/setpublic", method POST setPublic),
    ("/:gameid/:session/connect/:role", method POST connect),
    ("/:gameid/:session/shoot", method POST shoot),
    ("/:gameid/:session/chat", method POST sendMessage),
    ("/:gameid/:session/chat", method GET readMessages)
  ]

createGame :: Handler b GameService ()
createGame = do
  ownerName <- getParam "name"
  message <- getParam "message"
  body <- readRequestBody 4096
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ [ownerName, message]
  writeLBS . encode $ body
  modifyResponse $ setResponseCode 201

getPublicGamesList :: Handler b GameService ()
getPublicGamesList = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ ([Game 0 "test 1", Game 1 "test 2"])
  modifyResponse . setResponseCode $ 200

sendMap :: Handler b GameService ()
sendMap = do
  modifyResponse . setResponseCode $ 202

getStatus :: Handler b GameService ()
getStatus = do
  modifyResponse . setResponseCode $ 200

inviteBot :: Handler b GameService ()
inviteBot = do
  modifyResponse . setResponseCode $ 501

setPublic :: Handler b GameService ()
setPublic = do
  modifyResponse . setResponseCode $ 200

connect :: Handler b GameService ()
connect = do
  modifyResponse . setResponseCode $ 202

shoot :: Handler b GameService ()
shoot = do
  modifyResponse . setResponseCode $ 202

sendMessage :: Handler b GameService ()
sendMessage = do
  modifyResponse . setResponseCode $ 201

readMessages :: Handler b GameService ()
readMessages = do
  modifyResponse . setResponseCode $ 200

gameServiceInit :: MongoParams -> SnapletInit b GameService
gameServiceInit (MongoParams mongoHost mongoPort mongoUser mongoPass mongoDatabase)= makeSnaplet "game" "Battleship Service" Nothing $ do
  addRoutes gameRoutes
  return $ GameService
