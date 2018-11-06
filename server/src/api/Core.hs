{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Api.Services.GameService
import Control.Lens
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B

data Api = Api { _gameService :: Snaplet GameService }

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = do
  modifyResponse . setResponseCode $ 200

apiInit :: String -> String -> String -> String -> SnapletInit b Api
apiInit mongoHost mongoUser mongoPass mongoDb = makeSnaplet "api" "Core Api" Nothing $ do
        ts <- nestSnaplet "games" gameService $ gameServiceInit mongoHost mongoUser mongoPass mongoDb
        addRoutes apiRoutes
        return $ Api ts
