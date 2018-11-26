{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Play where

import qualified Data.Text as T
import Control.Exception
import Control.Concurrent (threadDelay)
import Types
import Database.MongoDB
import Database.MongoDB.Query as MQ
import Database.MongoDB.Connection
import Network.Connection
import Network.HTTP.Types
import Network.HTTP.Conduit
import Data.Aeson
import Data.Bson as BS
import System.Random
import Data.List

mapWidth :: Int
mapWidth = 10

mapHeight :: Int
mapHeight = 10

readGamesList :: String -> String -> IO [String]
readGamesList url name = do
      request <- parseRequest $ url ++ "/bots/" ++ name ++ "/games"
      let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      manager <- newManager settings
      res <- fmap responseBody $ httpLbs request manager
      let mbgames = decode $ res :: Maybe [String]
      case mbgames of (Just games) -> return games
                      _ -> return ([]::[String])

processGame ::  String -> Pipe -> Database -> [Rule] -> String -> IO ()
processGame url pipe db rules gid = do
  -- for gid I need to get game from MongoDB. Case it's not exists, connect and save in DB, 
  -- otherwise get status from url and case status config and player map is not exists - build map and send
  -- case player - shoot and save status
  -- otherwise - do nothing.
      mbGame <- performAction pipe db $ MQ.findOne (MQ.select ["game" =: gid] "games")
      case mbGame of Just game -> do
                             let session = at "session" game
                             request <- parseRequest $ url ++ "/" ++ gid ++ "/" ++ session
                             let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
                             manager <- newManager settings
                             res <- fmap responseBody $ httpLbs request manager
                             let mbgi = decode $ res :: Maybe GameInfo
                             let getShips rn = [s | (Rule n s) <- rules, n == rn]
                             case mbgi of (Just (GameInfo r "config" _ False)) -> do
                                                case (getShips r) of 
                                                  [] -> return ()
                                                  (shipset:_) -> sendShips 
                                                                      (url ++ "/" ++ gid ++ "/" ++ session ++ "/setmap")
                                                                      shipset
                                          (Just (GameInfo r "player" em _)) -> do
                                                case (getShips r) of 
                                                  [] -> return ()
                                                  (shipset:_) -> do
                                                        mbx <- try (BS.look "x" game) :: IO (Either SomeException BS.Value)
                                                        let url' = (url ++ "/" ++ gid ++ "/" ++ session ++ "/shoot")
                                                        case mbx of 
                                                          Right (BS.Int32 x') -> do -- we have unfinished ship
                                                                let x = (fromIntegral x') :: Int
                                                                let y = (at "y" game) :: Int
                                                                nextShot url' pipe db shipset (Point x y) em
                                                          _ -> newShot url' pipe db shipset em
                                          _ -> return ()
                     _ -> do
                       request' <- parseRequest $ url ++ "/" ++ gid ++ "/connect/player"
                       let request = request' { requestHeaders = [ (hContentType, "application/json")
                                                                 ]
                                              , method = "POST"
                                              , requestBody = RequestBodyLBS $ encode $ 
                                                    ConnectInfo "ILYA" "Hi, I'm bot! It's a pleasure to play with you!"
                                              }
                       let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
                       manager <- newManager settings
                       res <- fmap responseBody $ httpLbs request manager
                       let mbgames = decode $ res :: Maybe ApiNewGame
                       case mbgames of (Just (ApiNewGame gameId session)) -> do
                                             let gameinfo = [ "game" =: gid
                                                            , "session" =: session
                                                            ]::Document
                                             performAction pipe db $ MQ.insert "games" gameinfo
                                             return ()
                                       _ -> return ()

-- need to figure out where to shoot.
-- In case miss, save latest shot in DB.
-- In case sank, remove everything from DB and perform new shot.
nextShot :: String -> Pipe -> Database -> [[Int]] -> Point -> [[Int]] -> IO ()
nextShot url pipe db ships (Point x y) sea = do
      return ()

-- need to find living longest ship, places where it can have start and get random from these places
newShot :: String -> Pipe -> Database -> [[Int]] -> [[Int]] -> IO ()
newShot url pipe db ships sea = do
      let longest = getLongestSize ships sea
      let coords = (getAllCoordsForShip longest sea) ++ 
                   (map (\(Point x y) -> Point y x) (getAllCoordsForShip longest $ transpose sea))
      rand <- getStdRandom (randomR (0,length coords - 1))
      let coord = head $ drop rand coords ++ [Point 0 0]
      request' <- parseRequest $ url
      let request = request' { requestHeaders = [ (hContentType, "application/json")
                                                ]
                             , method = "POST"
                             , requestBody = RequestBodyLBS $ encode $ coord
                             }
      let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      manager <- newManager settings
      res <- fmap responseBody $ httpLbs request manager
      let result = decode $ res :: Maybe String
      case result of Just "hit" -> nextShot url pipe db ships coord $ placeShipInXY coord 1 sea
                     Just "sank" -> newShot url pipe db ships $ placeShipInXY coord 1 sea
                     _ -> return ()

getLongestSize :: [[Int]] -> [[Int]] -> Int
getLongestSize ships sea = head $ [s | (s:a:_) <- ships, a > (length [s' | s' <- proj, s'==s])] ++ [1]
                           where proj = (getProjection sea) ++ (getProjection . transpose $ sea)

getProjection :: [[Int]] -> [Int]
getProjection m = concat $ [foldr (\x (y:ys) -> case x of 
                                                   0 -> [0] ++ (y:ys)
                                                   _ -> (y+1:ys)) [0] $ l | l <- m]

initSea :: [[Int]]
initSea = replicate mapHeight $ replicate mapWidth 0

sendShips :: String -> [[Int]] -> IO ()
sendShips url shipset = do
      sea <- placeShips initSea shipset
      request' <- parseRequest $ url
      let request = request' { requestHeaders = [ (hContentType, "application/json")
                                                ]
                             , method = "POST"
                             , requestBody = RequestBodyLBS $ encode $ sea
                             }
      let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
      manager <- newManager settings
      res <- httpLbs request manager
      return ()

placeShips :: [[Int]] -> [[Int]] -> IO [[Int]]
placeShips sea [] = return sea
placeShips sea ((s:a:_):rs) = do 
  sea' <- placeShips' s a sea
  placeShips sea' rs

placeShips' :: Int -> Int -> [[Int]] -> IO [[Int]]
placeShips' _ 0 sea = return sea
placeShips' s a sea = do 
  sea' <- placeShip s sea
  placeShips' s (a - 1) sea' 

placeShip :: Int -> [[Int]] -> IO [[Int]]
placeShip s sea = do
  let tsea = transpose sea
  rand <- getStdRandom (randomR (0,1)) :: IO Int
  case rand of 0 -> do 
                      coords <- getRandomCoord s sea
                      return $ placeShipInXY coords s sea
               1 -> do
                      coords <- getRandomCoord s tsea
                      return $ placeShipInXY coords s tsea

placeShipInXY :: Point -> Int -> [[Int]] -> [[Int]]
placeShipInXY (Point x y) s sea = (take x sea) ++ 
                                  [(take y line) ++ (replicate s 1) ++ (drop (y+s) line)] ++ 
                                  (drop (x+1) sea)
                                  where line = head $ drop x sea

getRandomCoord :: Int -> [[Int]] -> IO Point
getRandomCoord s sea = do
  let coords = getAllCoordsForShip s sea
  rand <- getStdRandom (randomR (0,length coords - 1))
  return . head $ drop rand coords

getAllCoordsForShip :: Int -> [[Int]] -> [Point]
getAllCoordsForShip s sea = concat
                            [[Point x y | (v,y) <- zip (take (length l - s) l) [0,1..], v==0, checkShipsNear x y s sea] | 
                            (l,x) <- zip sea [0,1..]]

checkShipsNear :: Int -> Int -> Int -> [[Int]] -> Bool
checkShipsNear x y s sea = and $ map (\v -> v==0 || v==2) $ concat $ 
                           map (take (s+2) . drop (y-1)) (take 3 . drop (x-1) $ sea)

checkIfEmpty :: Int -> Int -> Int -> [[Int]] -> Bool
checkIfEmpty x y s sea = and $ map (==0) $ concat $ 
                           map (take (s+2) . drop (y-1)) (take 1 . drop x $ sea)

play :: Int -> String -> String -> String -> String -> String -> String -> [Rule] -> IO()
play repeatDelay apiurl botname smongoHost smongoUser smongoPass smongoDb rules = do
      let mongoHost = readHostPort smongoHost
      let mongoUser = T.pack smongoUser
      let mongoPass = T.pack smongoPass
      let mongoDb = T.pack smongoDb
      play' repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb rules

play' :: Int -> String -> String -> Host -> Username -> Password -> Database -> [Rule] -> IO()
play' repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb rules = do
      pipe <- connectAndAuth mongoHost mongoUser mongoPass mongoDb
      gamesList <- readGamesList apiurl botname
      mapM_ (processGame apiurl pipe mongoDb rules) gamesList
      closeConnection pipe
      threadDelay (max repeatDelay 1 * 1000000)
      play' repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb rules

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
