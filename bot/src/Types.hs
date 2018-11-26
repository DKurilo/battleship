{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad

data Point = Point { px :: Int,
                     py :: Int
                   } deriving (Show)
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

data Person = Person {pMap :: [[Int]]}
instance FromJSON Person where
  parseJSON (Object v) =
    Person <$> v .: "map"

data GameInfo' = GameInfo' { gisRules :: String
                           , gisTurn :: String
                           , gisEnemny :: Person
                           , gisMe :: Person
                           }
instance FromJSON GameInfo' where
  parseJSON (Object v) =
    GameInfo' <$> v .: "rules"
              <*> v .: "turn"
              <*> v .: "owner"
              <*> v .: "player"
  parseJSON _ = mzero



data GameInfo = GameInfo { giRules :: String
                         , giTurn :: String
                         , giEnemyMap :: [[Int]]
                         , giSentMap :: Bool
                         }
instance FromJSON GameInfo where
  parseJSON (Object v) =
    fmap 
      (\(GameInfo' r t (Person em) (Person mm)) -> GameInfo r t em $ length mm > 0)
      ((parseJSON (Object v)) :: (Parser GameInfo'))
  parseJSON _ = mzero

data Rule = Rule { rName :: String
                 , rShips :: [[Int]]
                 }
instance FromJSON Rule where
  parseJSON (Object v) =
    Rule <$> v .: "name"
         <*> v .: "ships"
  parseJSON _ = mzero
