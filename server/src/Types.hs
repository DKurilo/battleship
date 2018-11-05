{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

data MongoParams = MongoParams { host::String, port::String, username::String, password::String, database::String }
data MyConfig = MyConfig { staticPath::String, mongo::MongoParams }
