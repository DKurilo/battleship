{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Play where

import Control.Concurrent (threadDelay)

play :: Int -> String -> String -> String -> String -> String -> String -> IO()
play repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb = do
      -- mapM_ (processGame apiurl) $ readGamesList apiurl botname
      threadDelay (max repeatDelay 1 * 1000000)
      play repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb
