{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Control.Monad

data Game = Game { gameId :: String
                 }
instance FromJSON Game where
  parseJSON (Object v) =
    Game <$> v .: "id"
  parseJSON _ = mzero
instance ToJSON Game where
  toJSON (Game i) = object ["id" .= i]
