module Main where

import Network.Wai.Handler.Warp

import Complete (api)

main :: IO ()
main = run 8000 api
