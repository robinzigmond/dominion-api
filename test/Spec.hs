{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Servant.QuickCheck
import Test.Hspec
import Test.Hspec.Wai.QuickCheck
import Web.HttpApiData (LenientData(..))

import Api (publicAPI, server)
import Complete (api, DominionAPI)
import qualified SubsidiaryTypes as S


withApi :: IO () -> IO ()
withApi action =
    -- we can spin up a server in another thread and kill that thread when done
    -- in an exception-safe way
    bracket (liftIO $ forkIO $ run 8888 api) killThread (const action)


spec :: Spec
spec = apiTestRequestsSpec >> quickCheckSpec


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


-- QuickCheck tests

instance Arbitrary S.Set where
    arbitrary = fmap toEnum arbitrary


instance Arbitrary S.CardType where
    arbitrary = fmap toEnum arbitrary


instance Arbitrary S.CanDoItQueryChoice where
    arbitrary = fmap toEnum arbitrary


instance Arbitrary Text where
    arbitrary = fmap pack arbitrary


instance (Arbitrary a) => Arbitrary (LenientData a) where
    arbitrary = fmap (LenientData . Right) arbitrary


args :: Args
args = defaultArgs { maxSuccess = 500, chatty = True }


-- fails, unfortunately, and from a variety of different routes - despite those same routes working
-- fine in ALL manual tests
quickCheckSpec :: Spec
quickCheckSpec = describe "QuickCheck global tests for public API -" $ do
    it "API never gives 500 error" $
        withServantServer publicAPI (return server) $ \burl -> 
            serverSatisfies publicAPI burl args (not500 <%> mempty)


main :: IO ()
main = hspec spec
