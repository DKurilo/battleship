{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Internal as BL
import Data.ByteString.Lazy.Char8 as Char8
import Data.Bson as BS
import Data.Aeson

data Game = Game
  { 
    gameId :: Int,
    text :: T.Text
  }

instance ToJSON Game where
  toJSON (Game g t) = object [ "id" .= g, "text" .= t ]

instance ToJSON B.ByteString where
  toJSON b = toJSON $ B.unpack b

instance ToJSON BL.ByteString where
  toJSON b = toJSON $ Char8.unpack b

instance ToJSON BS.Field where
  toJSON f = "field"
