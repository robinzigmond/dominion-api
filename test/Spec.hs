{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Test.Hspec

import Complete (api, DominionAPI)
import Database (runTestDB)
import qualified SubsidiaryTypes as S


withApi :: IO () -> IO ()
withApi action =
    -- we can spin up a server in another thread and kill that thread when done
    -- in an exception-safe way
    bracket (liftIO . forkIO . run 8888 $ api runTestDB) killThread (const action)


spec :: Spec
spec = apiTestRequestsSpec


apiTestRequestsSpec :: Spec
apiTestRequestsSpec = around_ withApi $ do
    let private :<|> public :<|> docs = client (Proxy :: Proxy DominionAPI)
    let getAll :<|> filter :<|> getOne :<|> getSets :<|> getTypes = public
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8888"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    describe "general GET requests:" $ do
        it "the /cards route should succeed and result in some cards" $ do
            result <- runClientM getAll clientEnv
            getResultList result `shouldNotSatisfy` null
        it "getting a single card which exists should return 1 result" $ do
            result <- runClientM (getOne "copper") clientEnv
            getResultSingle result `shouldNotBe` Nothing
        it "getting a single card with doesn't exist should give an error" $ do
            result <- runClientM (getOne "fake-card") clientEnv
            getError result  `shouldNotBe` Nothing
        -- tests for filters, singly and in combination
        -- tests for linked cards being all present and correct, in each type of request
        -- test for no repeats in arrays of types/links

        where getResultSingle = either (const Nothing) S.result
              getResultList = either (const []) (fromMaybe [] . S.result)
              getError = either (const Nothing) S.error


main :: IO ()
main = hspec spec
