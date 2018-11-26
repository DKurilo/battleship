{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Control.Monad

data Point = Point { x :: Int,
                     y :: Int
                   }
instance FromJSON Point where
  parseJSON (Object v) =
    Point <$> v .: "x"
          <*> v .: "y"
  parseJSON _ = mzero
instance ToJSON Point where
  toJSON (Point x y) = object [ "x" .= x, "y" .= y ]

data ApiNewGame = ApiNewGame { angGameId :: String
                             , angSession :: String
                             }
instance FromJSON ApiNewGame where
  parseJSON (Object v) =
    ApiNewGame <$> v .: "game"
               <*> v .: "session"
  parseJSON _ = mzero

data ConnectInfo = ConnectInfo { ciName :: String
                               , ciMessage :: String
                               }
instance ToJSON ConnectInfo where
  toJSON (ConnectInfo n m) = object [ "name" .= n, "message" .= m ]
