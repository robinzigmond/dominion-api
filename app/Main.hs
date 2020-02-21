module Main where

import           Network.Wai.Handler.Warp
import           System.Environment       (getEnv)

import           Complete                 (api)
import           Database                 (isProd, runLiveDB)

main :: IO ()
main = do
  prod <- isProd
  port <-
    if prod
      then read <$> getEnv "PORT"
      else return 8000
  run port $ api runLiveDB
