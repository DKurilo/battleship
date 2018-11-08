{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Services.GameService where

import Control.Exception
import Control.Monad
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
import Snap.Snaplet as SN
import qualified Data.ByteString.Char8 as B
import Data.Text as T
import Data.Time.Clock.POSIX

data GameService = GameService { }

makeLenses ''GameService

gemeTimeout :: Int
gemeTimeout = 3600
---------------------
-- Routes
gameRoutes :: Host -> Username -> Password -> Database -> [(B.ByteString, SN.Handler b GameService ())]
gameRoutes mongoHost mongoUser mongoPass mongoDb = [
    ("/", method GET $ getPublicGamesList mongoHost mongoUser mongoPass mongoDb),
    ("/", method POST $ createGame mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/setmap", method POST $ sendMap mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session", method GET $ getStatus mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/invitebot", method POST $ inviteBot mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/setpublic", method POST $ setPublic mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/connect/player", method POST $ connectGamePlayer mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/connect/guest", method POST $ connectGameGuest mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/shoot", method POST $ shoot mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/chat", method POST $ sendMessage mongoHost mongoUser mongoPass mongoDb),
    ("/:gameid/:session/chat", method GET $ readMessages mongoHost mongoUser mongoPass mongoDb)
  ]

-------------------------
-- Actions

---------------------------
-- get list of opened
--   sends nothing
--   GET /api/games/
--   response list of {game id, messsge}
--   200
--   [
--     {
--       "game": {gameid},
--       "owner": {name},
--       "message": {game message}
--     },
--     ...
--   ]
--   500
--   {message}
getPublicGamesList :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
getPublicGamesList mongoHost mongoUser mongoPass mongoDb = do
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  modifyResponse $ setHeader "Content-Type" "application/json"
  time <- liftIO $ fmap round getPOSIXTime
  let action = rest =<< MQ.find (MQ.select ["date" =: ["$gte" =: time - gemeTimeout], "public" =: True] "games")
  games <- a $ action
  writeLBS . encode $ fmap (\d -> PublicGame (BS.at "game" d) (BS.at "name" (BS.at "owner" d)) (BS.at "message" d)) games
  liftIO $ closeConnection pipe
  modifyResponse . setResponseCode $ 200


----------------------------
-- create game 
--   post username, message
--   POST /api/games
--   {
--     "username": {username},
--     "message": {message}
--   }
--   response new game id and session or error
--   201
--   {
--     "game": {gameid},
--     "session": {session}
--   }
--   400, 500
--   {message}
createGame :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
createGame mongoHost mongoUser mongoPass mongoDb = do
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  modifyResponse $ setHeader "Content-Type" "application/json"
  user <- fmap (\x -> decode x :: Maybe NewGameUser) $ readRequestBody 4096
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
                              "public" =: False
                            ]
                 a $ MQ.insert "games" game
                 writeLBS $ encode $ NewGame gameId sessionId
                 modifyResponse $ setResponseCode 201
               Nothing -> do
                 writeLBS . encode $ APIError "Name and message can't be empty!"
                 modifyResponse $ setResponseCode 400
  liftIO $ closeConnection pipe

sendMap :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
sendMap mongoHost mongoUser mongoPass mongoDb = do
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  modifyResponse . setResponseCode $ 202
  liftIO $ closeConnection pipe

getStatus :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
getStatus mongoHost mongoUser mongoPass mongoDb = do
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  modifyResponse . setResponseCode $ 200
  liftIO $ closeConnection pipe

inviteBot :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
inviteBot mongoHost mongoUser mongoPass mongoDb = do
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  modifyResponse . setResponseCode $ 501
  liftIO $ closeConnection pipe

----------------------------
-- invite stranger
--   post game id and session (only owner can invite strangers) and message
--   POST /api/games/{gameid}/{session}/setpublic
--   {
--     "message": {message}
--   }
--   response success if added in list or error
--   200
--   "ok"
--   404, 500
--   {error}
setPublic :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
setPublic mongoHost mongoUser mongoPass mongoDb = do
  time <- liftIO $ fmap round getPOSIXTime
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  pgame <- getParam "gameid"
  session <- getParam "session"
  message <- fmap (\x -> decode x :: Maybe Message) $ readRequestBody 4096
  case message of 
        Just (Message msg) -> do
              let game = case pgame of Just g -> B.unpack g
                                       Nothing -> ""
              rights <- liftIO $ fillRights pipe mongoDb game (B.unpack <$> session)
              case rights of
                GameRights True True _ NOTREADY _ n False -> do
                  let act = [(
                               [
                                 "game" =: game
                               ]::Selector,
                               [
                                 "$set" =: ["public" =: True, "message" =: msg],
                                 "$push" =: ["chat" =: [ "name" =: n, "time" =: time, "message" =: "Attention! Game is public now!"]]
                               ]::Document,
                               [ ]::[UpdateOption]
                            )]
                  a $ MQ.updateAll "games" act
                  writeLBS "ok"
                  modifyResponse . setResponseCode $ 200
                _ -> do
                  writeLBS . encode $ APIError "Can't make this game public!"
                  modifyResponse $ setResponseCode 400
        _ -> do
              writeLBS . encode $ APIError "Can't find message!"
              modifyResponse $ setResponseCode 400
  liftIO $ closeConnection pipe

---------------------------------
-- connect
--   post game id, username, role (guest|player), short message
--   POST /api/games/{gameid}/connect/{guest|player}
--   {
--     "name": "name",
--     "message": "message"
--   }
--   response session, or error.
--   202
--   {
--     "game": {game}
--     "session": {session}
--   }
--   404, 403, 400, 500
--   {message}
connectGamePlayer :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
connectGamePlayer mongoHost mongoUser mongoPass mongoDb = do
  time <- liftIO $ fmap round getPOSIXTime
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  pgame <- getParam "gameid"
  player <- fmap (\x -> decode x :: Maybe NewGameUser) $ readRequestBody 4096
  case player of 
        Just (NewGameUser name message) -> do
              let game = case pgame of Just g -> B.unpack g
                                       Nothing -> ""
              rights <- liftIO $ fillRights pipe mongoDb game Nothing
              case rights of
                GameRights True False False NOTREADY _ _ _ -> do
                  sessionId <- liftIO $ UUID.toString <$> nextRandom
                  let act = [(
                               [
                                 "game" =: game
                               ]::Selector,
                               [
                                 "$set" =: ["player" =: ["name" =: name, "message" =: message, "session" =: sessionId], "turn" =: "config"],
                                 "$push" =: ["chat" =: [ "name" =: name, "time" =: time, "message" =: ("Welcome " ++ name ++ ". You've joined as a player!")]]
                               ]::Document,
                               [ ]::[UpdateOption]
                            )]
                  a $ MQ.updateAll "games" act
                  writeLBS $ encode $ NewGame game sessionId
                  modifyResponse . setResponseCode $ 200
                _ -> do
                  writeLBS . encode $ APIError "Can't connect as player!"
                  modifyResponse $ setResponseCode 400
        _ -> do
              writeLBS . encode $ APIError "Name and message are required!"
              modifyResponse $ setResponseCode 400
  liftIO $ closeConnection pipe

connectGameGuest :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
connectGameGuest mongoHost mongoUser mongoPass mongoDb = do
  time <- liftIO $ fmap round getPOSIXTime
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  pgame <- getParam "gameid"
  player <- fmap (\x -> decode x :: Maybe NewGameUser) $ readRequestBody 4096
  case player of 
        Just (NewGameUser name message) -> do
              let game = case pgame of Just g -> B.unpack g
                                       Nothing -> ""
              rights <- liftIO $ fillRights pipe mongoDb game Nothing
              case rights of
                GameRights True _ _ _ _ _ _ -> do
                  sessionId <- liftIO $ UUID.toString <$> nextRandom
                  let act = [(
                               [
                                 "game" =: game
                               ]::Selector,
                               [
                                 "$push" =: [
                                              "chat" =: [ "name" =: name, "time" =: time, "message" =: ("Welcome " ++ name ++ ". You've joined as a guest!")],
                                              "guests" =: ["name" =: name, "message" =: message, "session" =: sessionId]
                                            ]
                               ]::Document,
                               [ ]::[UpdateOption]
                            )]
                  a $ MQ.updateAll "games" act
                  writeLBS $ encode $ NewGame game sessionId
                  modifyResponse . setResponseCode $ 200
                _ -> do
                  writeLBS . encode $ APIError "Can't connect as guest!"
                  modifyResponse $ setResponseCode 400
        _ -> do
              writeLBS . encode $ APIError "Name and message are required!"
              modifyResponse $ setResponseCode 400
  liftIO $ closeConnection pipe

shoot :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
shoot mongoHost mongoUser mongoPass mongoDb = do
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  modifyResponse . setResponseCode $ 202
  liftIO $ closeConnection pipe

sendMessage :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
sendMessage mongoHost mongoUser mongoPass mongoDb = do
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  modifyResponse . setResponseCode $ 201
  liftIO $ closeConnection pipe

readMessages :: Host -> Username -> Password -> Database -> SN.Handler b GameService ()
readMessages mongoHost mongoUser mongoPass mongoDb = do
  pipe <- liftIO $ connectAndAuth mongoHost mongoUser mongoPass mongoDb
  let a action = liftIO $ performAction pipe mongoDb action
  modifyResponse . setResponseCode $ 200
  liftIO $ closeConnection pipe

----------------------
-- Game authentication
fillRights :: Pipe -> Database -> String -> Maybe String -> IO GameRights
fillRights pipe mongoDb game session = do
  let a action = liftIO $ performAction pipe mongoDb action
  time <- liftIO $ fmap round getPOSIXTime
  game <- a $ MQ.findOne (MQ.select ["date" =: ["$gte" =: time - gemeTimeout], "game" =: game] "games")
  let turn v = case v of Right (BS.String "owner") -> OWNER
                         Right (BS.String "player") -> PLAYER
                         Right (BS.String "finished") -> FINISHED
                         Right (BS.String "config") -> CONFIG
                         _ -> NOTREADY
  case game of 
    Just g -> 
      case session of
        Just sess -> do
          vturn <- try (BS.look "turn" g) :: IO (Either SomeException BS.Value)
          public <- try (BS.look "public" g) :: IO (Either SomeException BS.Value)
          let ispublic = case public of Right (BS.Bool p) -> p
                                        _ -> False
          owner <- try (BS.look "owner" g) :: IO (Either SomeException BS.Value)
          osess <- case owner of
              Right (BS.Doc d) -> BS.look "session" d
              _ -> return $ BS.Bool False
          let isowner = case osess of (BS.String s) -> (T.unpack s) == sess
                                      _ -> False
          player <- try (BS.look "player" g) :: IO (Either SomeException BS.Value)
          psess <- case player of
              Right (BS.Doc d) -> BS.look "session" d
              _ -> return $ BS.Bool False
          let isplayer = case psess of (BS.String s) -> (T.unpack s) == sess
                                       _ -> False
          guests <- try (BS.look "guests" g) :: IO (Either SomeException BS.Value)
          let isguest = case guests of 
                Right (BS.Array a) -> and $ fmap (\g -> 
                              (case g of (BS.Doc dg) -> (T.unpack (BS.at "session" dg)) == sess
                                         _ -> False)) a
                _ -> False
          let uname = case isowner of
                True -> T.unpack $ BS.at "session" $ BS.at "owner" g
                False -> case isplayer of
                      True -> T.unpack $ BS.at "session" $ BS.at "player" g
                      False -> case isguest of
                            True -> T.unpack $ BS.at "name" $ Prelude.head $ Prelude.filter (\x -> (BS.at "session" x) == sess) $ BS.at "guests" g
                            False -> ""
          return $ GameRights True isowner isplayer (turn vturn) isguest uname ispublic
        Nothing -> do
          vturn <- try (BS.look "turn" g) :: IO (Either SomeException BS.Value)
          return $ GameRights True False False (turn vturn) False "" False
    Nothing -> return $ GameRights False False False OWNER False "" False

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

----------------------
-- Initialization
gameServiceInit :: String -> String -> String -> String -> SnapletInit b GameService
gameServiceInit mongoHost mongoUser mongoPass mongoDb = makeSnaplet "game" "Battleship Service" Nothing $ do
  addRoutes $ gameRoutes (readHostPort mongoHost) (T.pack mongoUser) (T.pack mongoPass) (T.pack mongoDb)
  return $ GameService
