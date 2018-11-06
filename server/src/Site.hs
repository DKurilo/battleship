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
import           Data.Configurator
import           Control.Lens
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
import           Application


------------------------------------------------------------------------------
-- | The application's routes.
routes :: String -> [(ByteString, Handler App App ())]
routes s = [ ("", serveDirectoryWith defaultDirectoryConfig s) ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "battleship" "Battleship application." Nothing $ do
  conf <- getSnapletUserConfig
  staticPath <- liftIO $ require conf "battleship.static.path"
  mongoHost <- liftIO $ require conf "battleship.mongo.host"
  mongoUser <- liftIO $ require conf "battleship.mongo.user"
  mongoPass <- liftIO $ require conf "battleship.mongo.pass"
  mongoDb <- liftIO $ require conf "battleship.mongo.db"
  a <- nestSnaplet "api" api $ apiInit mongoHost mongoUser mongoPass mongoDb
  addRoutes $ routes $ staticPath
  return $ App a
