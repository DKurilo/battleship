{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Api.Core
import           Control.Lens
-- import           Database.MongoDB
import           Data.ConfigFile
import           Control.Monad.Error
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Types
import           Application

getConfig :: IO (Either CPError MyConfig)
getConfig = do
              rv <- runErrorT $
                do
                  cp <- join $ liftIO $ readfile emptyCP ".env"
                  let x = cp
                  static <- get x "DEFAULT" "static"
                  mongoHost <- get x "DEFAULT" "mongoHost"
                  mongoPort <- get x "DEFAULT" "mongoPort"
                  mongoUser <- get x "DEFAULT" "mongoUser"
                  mongoPass <- get x "DEFAULT" "mongoPass"
                  mongoDatabase <- get x "DEFAULT" "mongoDatabase"
                  return $ MyConfig static $ MongoParams mongoHost mongoPort mongoUser mongoPass mongoDatabase
              return rv

getStaticPath :: Either CPError MyConfig -> String
getStaticPath (Right (MyConfig s _)) = s
getStaticPath (Left _) = "../client/build"

getMongoDBParams :: Either CPError MyConfig -> MongoParams
getMongoDBParams (Right (MyConfig _ m)) = m
getMongoDBParams (Left _) = MongoParams "127.0.0.1" "27017" "" "" ""

------------------------------------------------------------------------------
-- | The application's routes.
routes :: String -> [(ByteString, Handler App App ())]
routes s = [ ("", serveDirectoryWith defaultDirectoryConfig s) ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "battleship" "Battleship application." Nothing $ do
  c <- liftIO $ getConfig
  a <- nestSnaplet "api" api (apiInit $ getMongoDBParams c)
  addRoutes $ routes $ getStaticPath c
  return $ App a
