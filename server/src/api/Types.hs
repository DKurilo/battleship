{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Internal as BL
import Data.ByteString.Lazy.Char8 as Char8
import Data.Bson as BS
import Data.Aeson
import Control.Monad

data Turn = OWNER|PLAYER|NOTREADY|CONFIG|FINISHED

data GameRights = GameRights { grIsExists :: Bool
                             , grIsOwner :: Bool
                             , grIsPlayer :: Bool
                             , grTurn :: Turn
                             , grIsGuest :: Bool
                             , grMyName :: String
                             , grPublic :: Bool
                             }

data PublicGame = PublicGame { pgGameId :: String
                             , pgOwnerName :: String
                             , pgMessage :: String
                             }
instance ToJSON PublicGame where
  toJSON (PublicGame g o m) = object [ "game" .= g, "owner" .= o, "message" .= m ]

data NewGameUser = NewGameUser { nguName :: String
                               , nguMessage :: String
                               }
instance FromJSON NewGameUser where
  parseJSON (Object v) =
    NewGameUser <$> v .: "name"
                <*> v .: "message"
  parseJSON _ = mzero

data Message = Message { msgMessage :: String }
instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "message"
  parseJSON _ = mzero

data APIError = APIError { e :: String }
instance ToJSON APIError where
  toJSON (APIError e) = toJSON $ e

data NewGame = NewGame { ngGame :: String, ngSession :: String }
instance ToJSON NewGame where
  toJSON (NewGame g s) = object [ "game" .= g, "session" .= s ]

instance ToJSON B.ByteString where
  toJSON b = toJSON $ B.unpack b

instance ToJSON BL.ByteString where
  toJSON b = toJSON $ Char8.unpack b

instance ToJSON BS.Field where
  toJSON v = "field"

