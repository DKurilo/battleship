{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Configurator
import Play (play)

main :: IO ()
main = do
      conf <- load $ [Required "devel.bot.cfg"]
      repeatDelay <- (require conf "battleshipbot.repeat")::(IO Int)
      mongoHost <- (require conf "battleshipbot.mongo.host")::(IO String)
      mongoUser <- (require conf "battleshipbot.mongo.user")::(IO String)
      mongoPass <- (require conf "battleshipbot.mongo.pass")::(IO String)
      mongoDb <- (require conf "battleshipbot.mongo.db")::(IO String)
      apiurl <- (require conf "battleshipbot.battleship")::(IO String)
      botname <- (require conf "battleshipbot.botname")::(IO String)
      play repeatDelay apiurl botname mongoHost mongoUser mongoPass mongoDb
