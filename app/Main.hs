module Main where

import           Network.Wai.Handler.Warp
import           System.Environment       (getEnv)

import           Complete                 (api)
import           Database                 (runLiveDB)

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  run port $ api runLiveDB
