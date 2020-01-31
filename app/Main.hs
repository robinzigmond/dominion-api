module Main where

import Network.Wai.Handler.Warp

import Complete (api)
import Database (runLiveDB)

main :: IO ()
main = run 8000 $ api runLiveDB
