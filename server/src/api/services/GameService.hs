{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.GameService where

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
gameRoutes = [("/", method GET getPublicGamesList), ("/", method POST createGame)]

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

gameServiceInit :: SnapletInit b GameService
gameServiceInit = makeSnaplet "game" "Battleship Service" Nothing $ do
  addRoutes gameRoutes
  return $ GameService
