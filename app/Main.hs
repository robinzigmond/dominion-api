module Main where

import Network.Wai.Handler.Warp (runSettings, setPort, setLogger, defaultSettings)
import Network.Wai.Logger (withStdoutLogger)

import Complete (api)

main :: IO ()
main = withStdoutLogger $ \aplogger -> do
        let settings = setPort 8000 $ setLogger aplogger defaultSettings
        runSettings settings api
